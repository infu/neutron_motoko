(*
This module provides some convenience to assemble WASM instruction lists. The
features are

 * O(1) concatenation (using an explicit append tree internally)
 * Managing of label depths.
 * Some simple peephole optimizations.
*)

open Wasm_exts.Ast
open Wasm.Source
open Wasm_exts.Values

let combine_shifts const op = function
  | I32 opl, ({it = I32 l'; _} as cl), I32 opr, I32 r' when opl = opr ->
    let l, r = Int32.(to_int l', to_int r') in
    if (l >= 0 && l < 32 && r >= 0 && r < 32 && l + r < 32) then
      Some [{const with it = Const {cl with it = I32 (Int32.add l' r')}}; {op with it = Binary (I32 opl)}]
    else None
  | I64 opl, ({it = I64 l'; _} as cl), I64 opr, I64 r' when opl = opr ->
    let l, r = Int64.(to_int l', to_int r') in
    if (l >= 0 && l < 64 && r >= 0 && r < 64 && l + r < 64) then
      Some [{const with it = Const {cl with it = I64 (Int64.add l' r')}}; {op with it = Binary (I64 opl)}]
    else None
  | _ -> None

let dwarf_like_instrs is =
  let rec collect found = function
    | [] -> List.rev found
    | instr :: rest when Wasm_exts.Ast.is_dwarf_like instr.it ->
      collect (instr :: found) rest
    | _ :: rest -> collect found rest
  in
  collect [] is

(* Some simple peephole optimizations, to make the output code look less stupid *)
(* This uses a zipper.*)
let optimize : instr list -> instr list = fun is ->
  let rec go l r = match l, r with
    (* Combine adjacent Metas *)
    | {it = Meta m2; _} as n2 :: {it = Meta m1; _} :: l', r' ->
      let combined =
        let open Wasm_exts.Dwarf5.Meta in
        match m1, m2 with
        | StatementDelimiter _, StatementDelimiter _ -> m2
        | StatementDelimiter _, Grouped (StatementDelimiter _ :: t) -> Grouped (m2 :: t)
        | Grouped g1, Grouped g2 -> Grouped (g2 @ g1)
        | Grouped g1, _ -> Grouped (m2 :: g1)
        | _, Grouped g2 -> Grouped (g2 @ [m1])
        | _, _ -> Grouped [m2; m1] in
      go ({ n2 with it = Meta combined } :: l') r'

    (* Loading and dropping is pointless *)
    | { it = Const _ | LocalGet _; _} :: l', { it = Drop; _ } :: r' -> go l' r'
    (* Loading and dropping is pointless, even with intervening Meta *)
    | { it = Meta _; _} as m :: { it = Const _ | LocalGet _; _} :: l', { it = Drop; _ } :: r' -> go l' (m :: r')
    (* The following is not semantics preserving for general Wasm (due to out-of-memory)
       but should be fine for the code that we create *)
    | { it = Load _; _} :: l', { it = Drop; _ } :: _ -> go l' r
    (* Introduce LocalTee *)
    | { it = LocalSet n1; _} :: l', ({ it = LocalGet n2; _ } as i) :: r' when n1 = n2 ->
      go l' ({i with it = LocalTee n2 } :: r')
    (* Introduce LocalTee with previously intervening Meta *)
    | { it = Meta _; _} as m :: { it = LocalSet n1; _} :: l', ({ it = LocalGet n2; _ } as i) :: r' when n1 = n2 ->
      go l' (m :: {i with it = LocalTee n2 } :: r')
    (* Eliminate LocalTee followed by Drop (good for confluence) *)
    | ({ it = LocalTee n; _} as i) :: l', { it = Drop; _ } :: r' ->
      go l' ({i with it = LocalSet n } :: r')
    (* Eliminate Get followed by Set (typical artifact from the multi-value emulation) *)
    | { it = LocalGet n1; _} :: l', ({ it = LocalSet n2; _ }) :: r' when n1 = n2 ->
      go l' r'
    | { it = GlobalGet n1; _} :: l', ({ it = GlobalSet n2; _ }) :: r' when n1 = n2 ->
      go l' r'
    (* Code after Return, Br, BrTable or Unreachable is dead *)
    | _, ({ it = Return | Br _ | BrTable _ | Unreachable; _ } as i) :: t ->
      (* see Note [funneling DIEs through Wasm.Ast] *)
      List.rev_append (i :: l) (dwarf_like_instrs t)
    (* Equals zero has a dedicated operation (and works well with leg swapping) *)
    | ({it = Compare (I32 I32Op.Eq); _} as i) :: {it = Const {it = I32 0l; _}; _} :: l', r' ->
      go l' ({ i with it = Test (I32 I32Op.Eqz)} :: r')
    | ({it = Compare (I64 I64Op.Eq); _} as i) :: {it = Const {it = I64 0L; _}; _} :: l', r' ->
      go l' ({ i with it = Test (I64 I64Op.Eqz)} :: r')
    (* Constants before `Eqz` reduce trivially *)
    | ({it = Test (I32 I32Op.Eqz); _} as i) :: {it = Const {it = I32 n; _}; _} :: l', r' ->
      go l' ({ i with it = Const {it = I32 (if n = 0l then 1l else 0l); at = i.at}} :: r')
    | ({it = Test (I64 I64Op.Eqz); _} as i) :: {it = Const {it = I64 n; _}; _} :: l', r' ->
      go l' ({ i with it = Const {it = I32 (if n = 0L then 1l else 0l); at = i.at}} :: r')
    (* eqz after eq/ne becomes ne/eq *)
    | ({it = Test (I32 I32Op.Eqz); _} as i) :: {it = Compare (I32 I32Op.Eq); _} :: l', r' ->
      go l' ({ i with it = Compare (I32 I32Op.Ne)} :: r')
    | ({it = Test (I32 I32Op.Eqz); _} as i) :: {it = Compare (I32 I32Op.Ne); _} :: l', r' ->
      go l' ({ i with it = Compare (I32 I32Op.Eq)} :: r')
    | ({it = Test (I32 I32Op.Eqz); _} as i) :: {it = Compare (I64 I64Op.Eq); _} :: l', r' ->
      go l' ({ i with it = Compare (I64 I64Op.Ne)} :: r')
    | ({it = Test (I32 I32Op.Eqz); _} as i) :: {it = Compare (I64 I64Op.Ne); _} :: l', r' ->
      go l' ({ i with it = Compare (I64 I64Op.Eq)} :: r')
    | ({it = Test (I32 I32Op.Eqz); _} as i) :: {it = Compare (F32 F32Op.Eq); _} :: l', r' ->
      go l' ({ i with it = Compare (F32 F32Op.Ne)} :: r')
    | ({it = Test (I32 I32Op.Eqz); _} as i) :: {it = Compare (F32 F32Op.Ne); _} :: l', r' ->
      go l' ({ i with it = Compare (F32 F32Op.Eq)} :: r')
    | ({it = Test (I32 I32Op.Eqz); _} as i) :: {it = Compare (F64 F64Op.Eq); _} :: l', r' ->
      go l' ({ i with it = Compare (F64 F64Op.Ne)} :: r')
    | ({it = Test (I32 I32Op.Eqz); _} as i) :: {it = Compare (F64 F64Op.Ne); _} :: l', r' ->
      go l' ({ i with it = Compare (F64 F64Op.Eq)} :: r')
    (* LSBit masking before `If` is `Ctz` and switched `If` legs *)
    | ({ it = Binary (I32 I32Op.And); _} as a) :: { it = Const {it = I32 1l; _}; _} :: l', ({it = If (res,then_,else_); _} as i) :: r' ->
      go ({a with it = Unary (I32 I32Op.Ctz)} :: l') ({i with it = If (res,else_,then_)} :: r')
    | ({ it = Binary (I64 I64Op.And); _} as a) :: { it = Const {it = I64 1L; _}; _} :: l', ({it = If (res,then_,else_); _} as i) :: r' ->
      go ({a with it = Unary (I64 I64Op.Ctz)} :: l') ({i with it = If (res,else_,then_)} :: r')
    (* Same, with an intervening i32.wrap_i64 (semantically irrelevant for the LSB test) *)
    | ({ it = Convert (I32 I32Op.WrapI64); _} as w) :: ({ it = Binary (I64 I64Op.And); _} as a) :: { it = Const {it = I64 1L; _}; _} :: l', ({it = If (res,then_,else_); _} as i) :: r' ->
      go (w :: {a with it = Unary (I64 I64Op.Ctz)} :: l') ({i with it = If (res,else_,then_)} :: r')
    (* `br_if` variants: `[and 1; eqz; br_if]` collapses to `[ctz; br_if]`.
       Unlike the `if` forms above there is no leg to swap — `br_if` has only
       one branch direction, and `ctz` already inverts the LSB test the right way. *)
    | { it = Test (I32 I32Op.Eqz); _} :: ({ it = Binary (I32 I32Op.And); _} as a) :: { it = Const {it = I32 1l; _}; _} :: l', ({it = BrIf _; _} as br) :: r' ->
      go ({a with it = Unary (I32 I32Op.Ctz)} :: l') (br :: r')
    | ({ it = Test (I64 I64Op.Eqz); _} as e) :: ({ it = Binary (I64 I64Op.And); _} as a) :: { it = Const {it = I64 1L; _}; _} :: l', ({it = BrIf _; _} as br) :: r' ->
      go ({e with it = Convert (I32 I32Op.WrapI64)} :: {a with it = Unary (I64 I64Op.Ctz)} :: l') (br :: r')
    (* Same i64 form, with an intervening `i32.wrap_i64; i32.eqz` (instead of `i64.eqz`) *)
    | { it = Test (I32 I32Op.Eqz); _} :: ({ it = Convert (I32 I32Op.WrapI64); _} as w) :: ({ it = Binary (I64 I64Op.And); _} as a) :: { it = Const {it = I64 1L; _}; _} :: l', ({it = BrIf _; _} as br) :: r' ->
      go (w :: {a with it = Unary (I64 I64Op.Ctz)} :: l') (br :: r')
    (* `If` blocks after pushed constants are simplifiable *)
    | { it = Const {it = I32 0l; _}; _} :: l', ({it = If (res,_,else_); _} as i) :: r' ->
      go l' ({i with it = Block (res, else_)} :: r')
    | { it = Const {it = I32 _; _}; _} :: l', ({it = If (res,then_,_); _} as i) :: r' ->
      go l' ({i with it = Block (res, then_)} :: r')
    (* `If` blocks after negation can swap legs *)
    | { it = Test (I32 I32Op.Eqz); _} :: l', ({it = If (res,then_,else_); _} as i) :: r' ->
      go l' ({i with it = If (res,else_,then_)} :: r')
    (* `If` blocks with empty legs (or just a Br 0 fall-through) just drop *)
    | l', ({it = If (_, ([] | [{it = Br {it = 0l; _}; _}]), []); _} as i) :: r' ->
       go l' ({i with it = Drop} :: r')
    | l', ({it = If (_, [{it = Br x; _}], []); _} as i) :: r' ->
      go l' ({i with it = BrIf {x with it = Int32.sub x.it 1l}} :: r')
    (* `If` blocks with empty then after comparison can invert the comparison and swap legs *)
    | { it = Compare (I32 I32Op.Eq); _} as comp :: l', ({it = If (res,[],else_); _} as i) :: r' ->
      go ({comp with it = Compare (I32 I32Op.Ne)} :: l') ({i with it = If (res,else_,[])} :: r')
    | { it = (Compare (I32 I32Op.Ne) | Binary (I32 I32Op.Xor)); _} as comp :: l', ({it = If (res,[],else_); _} as i) :: r' ->
      go ({comp with it = Compare (I32 I32Op.Eq)} :: l') ({i with it = If (res,else_,[])} :: r')
    (* Empty block is redundant *)
    | l', ({ it = Block (_, []); _ }) :: r' -> go l' r'
    (* Constant shifts can be combined *)
    | {it = Binary (I32 I32Op.(Shl|ShrS|ShrU) as opl); _} :: {it = Const cl; _} :: l',
      ({it = Const cr; _} as const) :: ({it = Binary opr; _} as op) :: r'
        when Option.is_some (combine_shifts const op (opl, cl, opr, cr.it)) ->
      go l' (Option.get (combine_shifts const op (opl, cl, opr, cr.it)) @ r')
    | {it = Binary (I64 I64Op.(Shl|ShrS|ShrU) as opl); _} :: {it = Const cl; _} :: l',
      ({it = Const cr; _} as const) :: ({it = Binary opr; _} as op) :: r'
        when Option.is_some (combine_shifts const op (opl, cl, opr, cr.it)) ->
      go l' (Option.get (combine_shifts const op (opl, cl, opr, cr.it)) @ r')
    (* Examining topmost bit *)
    | {it = Binary (I32 I32Op.ShrU); _} as shift :: {it = Const {it = I32 31l; _}; _} :: l',
      ({it = If (res,then_,else_); _} as if_) :: r' ->
      go l' ({ shift with it = Unary (I32 I32Op.Clz) } :: { if_ with it = If (res,else_,then_) } :: r')
    | {it = Binary (I32 I32Op.And); _} as and_ :: {it = Const {it = I32 2147483648l; _}; _} :: l',
      ({it = If (res,then_,else_); _} as if_) :: r' ->
      go l' ({ and_ with it = Unary (I32 I32Op.Clz) } :: { if_ with it = If (res,else_,then_) } :: r')
    (* Null shifts can be eliminated *)
    | l', {it = Const {it = I32 0l; _}; _} :: {it = Binary (I32 I32Op.(Shl|ShrS|ShrU)); _} :: r' ->
      go l' r'
    | l', {it = Const {it = I64 0L; _}; _} :: {it = Binary (I64 I64Op.(Shl|ShrS|ShrU)); _} :: r' ->
      go l' r'
    (* Widen followed by narrow is pointless - but not the opposite! *)
    | {it = Convert (I64 I64Op.(ExtendSI32 | ExtendUI32)); _} :: l', {it = Convert (I32 I32Op.WrapI64); _} :: r'
    | {it = Convert (F64 F64Op.PromoteF32); _} :: l', {it = Convert (F32 F32Op.DemoteF64); _} :: r' ->
      go l' r'
    (* Constant bitwise `and` evaluation *)
    | l', {it = Const {it = I64 cl; _}; _} :: {it = Const {it = I64 cr; _}; _} :: {it = Binary (I64 I64Op.And); at} :: r' ->
      let combined = {it = Const {it = I64 (Int64.logand cl cr); at}; at} in
      go l' (combined :: r')
    (* Constant bitwise `or` evaluation *)
    | l', {it = Const {it = I64 cl; _}; _} :: {it = Const {it = I64 cr; _}; _} :: {it = Binary (I64 I64Op.Or); at} :: r' ->
      let combined = {it = Const {it = I64 (Int64.logor cl cr); at}; at} in
      go l' (combined :: r')
    (* Widen followed by I64.Eqz can be simplified to I32.Eqz *)
    | l', {it = Convert (I64 I64Op.(ExtendSI32 | ExtendUI32)); _} :: {it = Test (I64 I64Op.Eqz); at} :: r' ->
      go l' ({it = Test (I32 I32Op.Eqz); at} :: r')
    (* Narrow a constant *)
    | l', {it = Const {it = I64 c; _}; at} :: {it = Convert (I32 I32Op.WrapI64); _} :: r' ->
      let narrowed = {it = Const {it = I32 (Int64.to_int32 c); at}; at} in
      go l' (narrowed :: r')
    (* Look further *)
    | _, i::r' -> go (i::l) r'
    (* Done looking *)
    | l, [] -> List.rev l
  in go [] is

(* Remembered label depths are promises because builders can refer to a label
   before the instruction tree is materialized. *)
type depth = int32 Lib.Promise.t

(* The main type of this module is an explicit instruction-builder tree.

   In particular, [Append] replaces the old functional difference list.  A
   long chain of [(^^)] therefore allocates ordinary data instead of a chain of
   function calls that consumes the native or wasm_of_ocaml stack when it is
   materialized.  The structured nodes defer region and label-depth handling
   until the iterative materializer below. *)
type t =
  | Empty
  | Leaf of instr'
  | Append of t * t
  | WithRegion of Source.region * t
  | IfNode of block_type * t * t
  | BlockNode of block_type * t
  | LoopNode of t
  | RememberDepth of depth * t
  | BranchTo of depth

(* Continuation frames keep append trees and nested wasm bodies off the OCaml
   call stack. Link frames directly rather than allocating both a frame and a
   separate list cell per deferred fragment. Each saved instruction list is
   in reverse emission order. *)
type materialize_frame =
  | Done
  | ContinueWith of int32 * Source.region * t * materialize_frame
  | FinishBlock of instr list * Source.region * block_type * materialize_frame
  | FinishLoop of instr list * Source.region * materialize_frame
  | FinishThen of instr list * int32 * Source.region * block_type * t * materialize_frame
  | FinishIf of instr list * Source.region * block_type * instr list * materialize_frame

let optimized_reversed is =
  optimize (List.rev is)

(* Materialize a builder without optimizing its top-level instruction list.
   Nested wasm bodies are still optimized, matching the old combinators: they
   called [to_nested_list] before returning their enclosing instruction. *)
let materialize_raw_at depth pos (is : t) : instr list =
  let rec emit emitted d at current work =
    match current with
    | Empty ->
      resume emitted work
    | Leaf instr ->
      resume ((instr @@ at) :: emitted) work
    | Append (left, right) ->
      (* Descend into the current child directly. Only the deferred right
         sibling needs a heap continuation. *)
      emit emitted d at left (ContinueWith (d, at, right, work))
    | WithRegion (at, body) ->
      emit emitted d at body work
    | BlockNode (ty, body) ->
      emit [] (Int32.add d 1l) at body
        (FinishBlock (emitted, at, ty, work))
    | LoopNode body ->
      emit [] (Int32.add d 1l) at body
        (FinishLoop (emitted, at, work))
    | IfNode (ty, thn, els) ->
      emit [] (Int32.add d 1l) at thn
        (FinishThen (emitted, d, at, ty, els, work))
    | RememberDepth (label, body) ->
      (* This must happen before [body] is visited: it may start with a
         [BranchTo] that reads the promise. *)
      Lib.Promise.fulfill label d;
      emit emitted d at body work
    | BranchTo label ->
      let relative_depth = Int32.sub d (Lib.Promise.value label) in
      resume ((Br (relative_depth @@ at) @@ at) :: emitted) work
  and resume emitted = function
    | Done ->
      List.rev emitted
    | ContinueWith (d, at, body, work) ->
      emit emitted d at body work
    | FinishBlock (parent, at, ty, work) ->
      let body = optimized_reversed emitted in
      resume ((Block (ty, body) @@ at) :: parent) work
    | FinishLoop (parent, at, work) ->
      let body = optimized_reversed emitted in
      resume ((Loop (ValBlockType None, body) @@ at) :: parent) work
    | FinishThen (parent, d, at, ty, els, work) ->
      let thn = optimized_reversed emitted in
      emit [] (Int32.add d 1l) at els
        (FinishIf (parent, at, ty, thn, work))
    | FinishIf (parent, at, ty, thn, work) ->
      let els = optimized_reversed emitted in
      resume ((If (ty, thn, els) @@ at) :: parent) work
  in
  emit [] depth pos is Done

let materialize_raw is =
  materialize_raw_at 0l Wasm.Source.no_region is

let materialize_optimized_at depth pos is =
  optimize (materialize_raw_at depth pos is)

let to_instr_list (is : t) : instr list =
  materialize_optimized_at 0l Wasm.Source.no_region is

let to_nested_list d pos is =
  materialize_optimized_at Int32.(add d 1l) pos is


(* Do nothing *)
let nop : t = Empty

(* The concatenation operator *)
let (^^) (is1 : t) (is2 : t) : t =
  match is1, is2 with
  | Empty, is | is, Empty -> is
  | _ -> Append (is1, is2)

(* Singletons *)
let i (instr : instr') : t = Leaf instr

(* Map and concatenate without an intermediate mapped list or a recursive
   [fold_right].  These loops evaluate mapping functions in the same order as
   their former list-building counterparts. *)
let concat xs =
  let rec go acc = function
    | [] -> acc
    | is :: rest -> go (acc ^^ is) rest
  in
  go nop xs

let concat_map f xs =
  let rec go acc = function
    | [] -> acc
    | x :: rest ->
      let is = f x in
      go (acc ^^ is) rest
  in
  go nop xs

let concat_mapi f xs =
  let rec go index acc = function
    | [] -> acc
    | x :: rest ->
      let is = f index x in
      go (index + 1) (acc ^^ is) rest
  in
  go 0 nop xs

let table n f =
  (* [Lib.List.table] used to call [f] from [n - 1] down to zero while
     returning the fragments in ascending order.  Preserve both properties. *)
  let rec go index acc =
    if index < 0 then acc else
      let is = f index in
      go (index - 1) (is ^^ acc)
  in
  if n <= 0 then nop else go (n - 1) nop

(* Region-managing combinator *)

let with_region (pos : Source.region) (body : t) : t =
  match body with
  | Empty -> Empty
  | _ -> WithRegion (pos, body)

(* Depths-managing combinators *)

let if_ (ty : block_type) (thn : t) (els : t) : t =
  IfNode (ty, thn, els)

(* Shortcuts for unary and nullary variants *)
let if0 = if_ (ValBlockType None)
let if1 ty = if_ (ValBlockType (Some ty))

let block_ (ty : block_type) (body : t) : t =
  BlockNode (ty, body)
let block0 = block_ (ValBlockType None)
let block1 ty = block_ (ValBlockType (Some ty))

let loop0 (body : t) : t =
  LoopNode body

(* Remember depth *)
let new_depth_label () : depth =  Lib.Promise.make ()

let remember_depth depth (is : t) : t =
  RememberDepth (depth, is)

let with_current_depth (k : depth -> t) : t =
  let depth = new_depth_label () in
  remember_depth depth (k depth)

let with_current_depth' (k : depth -> ('a * t)) : ('a * t) =
  let depth = new_depth_label () in
  let x, is = k depth in
  (x, remember_depth depth is)

let branch_to_ (p : depth) : t =
  BranchTo p

(* Convenience combinators *)

let labeled_block1 ty depth (body : t) : t =
  block1 ty (remember_depth depth body)

(* Obtain the setter from a known variable's getter *)

let setter_for (getter : t) =
  match getter with
  | Leaf (LocalGet v) -> i (LocalSet v)
  | Leaf (GlobalGet v) -> i (GlobalSet v)
  | _ ->
    match materialize_raw getter with
    | [{it = LocalGet v; _}] -> i (LocalSet v)
    | [{it = GlobalGet v; _}] -> i (GlobalSet v)
    | _ -> failwith "input must be a getter"

(* Intended to be used within assert *)

let is_nop (is : t) =
  match is with
  | Empty -> true
  | _ -> materialize_raw is = []

(* DWARF tags and attributes: see Note [funneling DIEs through Wasm.Ast] *)

open Wasm_exts.Dwarf5
open Meta

open Die

(* Note [emit a DW_TAG]
   ~~~~~~~~~~~~~~~~~~~~
   There are two principal types of DWARF tags, those
   which are intimately tied to the generated instructions
   and those that are not. The latter ones include type
   definitions which can be regarded as global, while
   the former bracket or delimit instructions, like
   scoping or variable definitions. These are required to
   float with the instruction stream.

   Another aspect of tags is whether a tag admits children.
   When it admits children, these follow sequentially,
   closed by dw_tag_close. The abbreviation table must
   be consulted when deciding between
   - dw_tag_no_children, or
   - dw_tag.
   The former is self-closing (no nested tags), and the
   latter is high-level, straddling a region of code.
   The low-level alternative to the latter is using the
   dw_tag_open/dw_tag_close pair explicitly to demarcate
   the enclosed instructions. This moves the burden of
   balancing to the user.
   See also: Note [funneling DIEs through Wasm.Ast]
 *)

(* Note [locations for types]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~
   Motoko uses a variety of formats to store data depending on its type
   and even the actual value. Location  expressions are necessary to teach
   the debugger where the data can be found.
   - For short numeric types we have a location expression that LSB-aligns
     and leaves the value on the expression stack. This is a workaround for
     `lldb`'s insufficiency to understand basic types that are bitfields
     (http://lists.llvm.org/pipermail/lldb-dev/2020-August/016393.html).
     This also implies that setting of such variables in `lldb` won't work.
   - 32/64 bit-sized integrals may be stored on the heap, we use `DW_OP_bra`
     to guide the debugger after examining the indirection bit.
     We can't use the pointer key to be a discriminator for DW_TAG_variant_part
     because of lldb (link above), and probably displaying issues.
   - For arbitrary-precision integrals, we cannot inspect the multi-precision
     heap representation yet
   - Variants with no payload map to C-like `enum`s, the location expression
     takes care of focussing on the hash value
   - Variants map to `DW_TAG_variant_*` directly
   - Tuples may use Rust-like decoding
   - Objects need field search for members (when encoded as structure members)
   - The `Any` type will need fully dynamic resolution by `lldb`
   - Parameter types for polymorphic types/functions will be treated as `Any`.
   - `Text` will be treated as `Any` as it needs pretty-printing in the presence
     of concatenation nodes

 *)

(* injecting a tag into the instruction stream, see Note [emit a DW_TAG] *)
let dw_tag_open tag : t =
  let metas = concat_map (fun die -> i (Meta die)) in
  metas (tag_open tag)

let dw_tag die body =
  let dw_tag_close : t = i (Meta TagClose) in
  dw_tag_open die ^^ body ^^ dw_tag_close
let dw_tag_no_children = dw_tag_open (* self-closing *)

(* Marker for statement boundaries *)
let dw_statement { left; _ } =
  i (Meta (StatementDelimiter left))
