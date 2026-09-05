open Wasm_exts.Ast
open Wasm.Source
open Wasm_exts.Values
open Codegen.InstrList

let nr value = value @@ no_region

let i32 value =
  i (Const (nr (I32 (Int32.of_int value))))

let values_of_consts instrs =
  List.filter_map
    (function
      | {it = Const {it = I32 value; _}; _} -> Some (Int32.to_int value)
      | _ -> None)
    instrs

let make_left_skewed count =
  let rec go remaining acc =
    if remaining = 0 then acc else go (remaining - 1) (acc ^^ i Nop)
  in
  go count nop

let make_right_skewed count =
  let rec go remaining acc =
    if remaining = 0 then acc else go (remaining - 1) (i Nop ^^ acc)
  in
  go count nop

let left_skewed () =
  List.length (to_instr_list (make_left_skewed 100_000)) = 100_000

let right_skewed () =
  List.length (to_instr_list (make_right_skewed 100_000)) = 100_000

let large_concat_map () =
  let inputs = Lib.List.table 100_000 Fun.id in
  let output = to_instr_list (concat_map (fun _ -> i Nop) inputs) in
  List.length output = 100_000

let large_concat () =
  let fragments = Lib.List.table 100_000 (fun _ -> i Nop) in
  List.length (to_instr_list (concat fragments)) = 100_000

let large_terminating_body () =
  List.length (to_instr_list (make_left_skewed 100_000 ^^ i Return)) = 100_001

let concat_mapi_order () =
  let output =
    to_instr_list (concat_mapi (fun index _ -> i32 index) ['a'; 'b'; 'c'; 'd'])
  in
  values_of_consts output = [0; 1; 2; 3]

let table_orders () =
  let calls = ref [] in
  let body = table 4 (fun index -> calls := index :: !calls; i32 index) in
  !calls = [0; 1; 2; 3] &&
  values_of_consts (to_instr_list body) = [0; 1; 2; 3]

let regions () =
  let point = {file = "instrList_test.mo"; line = 3; column = 7} in
  let region = {left = point; right = point} in
  match to_instr_list (with_region region (block0 (i Nop))) with
  | [{it = Block (_, [{it = Nop; at = inner}]); at = outer}] ->
    outer = region && inner = region
  | _ -> false

let if_order () =
  match to_instr_list (if0 (i32 1) (i32 2)) with
  | [{it = If (_, [{it = Const {it = I32 1l; _}; _}],
                     [{it = Const {it = I32 2l; _}; _}]); _}] -> true
  | _ -> false

let remember_before_body () =
  let label = new_depth_label () in
  let body = block0 (remember_depth label (block0 (branch_to_ label))) in
  match to_instr_list body with
  | [{it = Block (_, [{it = Block (_, [{it = Br {it = 1l; _}; _}]); _}]); _}] -> true
  | _ -> false

let loop_branch () =
  let label = new_depth_label () in
  let body = loop0 (remember_depth label (branch_to_ label)) in
  match to_instr_list body with
  | [{it = Loop (_, [{it = Br {it = 0l; _}; _}]); _}] -> true
  | _ -> false

let outward_branch () =
  let body = with_current_depth (fun label -> block0 (branch_to_ label)) in
  match to_instr_list body with
  | [{it = Block (_, [{it = Br {it = 1l; _}; _}]); _}] -> true
  | _ -> false

let raw_setter () =
  match to_instr_list (setter_for (i (LocalGet (nr 7l)))) with
  | [{it = LocalSet {it = 7l; _}; _}] -> true
  | _ -> false

let raw_is_nop () =
  let body = i32 0 ^^ i Drop in
  not (is_nop body) && to_instr_list body = []

let nested_optimization () =
  match materialize_raw (block0 (i32 0 ^^ i Drop)) with
  | [{it = Block (_, []); _}] -> true
  | _ -> false

let deep_blocks () =
  let rec build remaining body =
    if remaining = 0 then body else build (remaining - 1) (block0 body)
  in
  let rec count remaining = function
    | [{it = Block (_, body); _}] -> count (remaining - 1) body
    | [{it = Nop; _}] -> remaining = 0
    | _ -> false
  in
  count 100_000 (to_instr_list (build 100_000 (i Nop)))

let deep_if_branches () =
  let rec build remaining body =
    if remaining = 0 then body else build (remaining - 1) (if0 body (i32 2))
  in
  let rec count remaining = function
    | [{it = If (_, body, [{it = Const {it = I32 2l; _}; _}]); _}] ->
      count (remaining - 1) body
    | [{it = Nop; _}] -> remaining = 0
    | _ -> false
  in
  count 100_000 (to_instr_list (build 100_000 (i Nop)))

let restored_region_and_depth () =
  let point = {file = "instrList_test.mo"; line = 11; column = 4} in
  let region = {left = point; right = point} in
  let label = new_depth_label () in
  let body = remember_depth label
    (with_region region (block0 (branch_to_ label)) ^^ branch_to_ label)
  in
  match materialize_raw body with
  | [{it = Block (_, [{it = Br {it = 1l; _}; at = inner}]); at = outer};
     {it = Br {it = 0l; _}; at = sibling}] ->
    outer = region && inner = region && sibling = no_region
  | _ -> false

(* A deliberately simple recursive reference for small generated trees. The
   production implementation must preserve these raw/nested-optimized
   semantics without using a call frame or a separate list cell per pending
   builder. Generate a fresh tree for each materializer: remembered depth
   promises are deliberately single-use. *)
let rec reference_raw d at = function
  | Empty -> []
  | Leaf instr -> [instr @@ at]
  | Append (left, right) ->
    let left = reference_raw d at left in
    let right = reference_raw d at right in
    left @ right
  | WithRegion (at, body) -> reference_raw d at body
  | BlockNode (ty, body) ->
    [Block (ty, reference_nested d at body) @@ at]
  | LoopNode body ->
    [Loop (ValBlockType None, reference_nested d at body) @@ at]
  | IfNode (ty, thn, els) ->
    let thn = reference_nested d at thn in
    let els = reference_nested d at els in
    [If (ty, thn, els) @@ at]
  | RememberDepth (label, body) ->
    Lib.Promise.fulfill label d;
    reference_raw d at body
  | BranchTo label ->
    [Br (Int32.sub d (Lib.Promise.value label) @@ at) @@ at]
and reference_nested d at body =
  optimize (reference_raw (Int32.add d 1l) at body)

let random_builder seed =
  let random = Random.State.make [|seed|] in
  let pick n = Random.State.int random n in
  let leaf () =
    match pick 7 with
    | 0 -> i32 (pick 4)
    | 1 -> i (LocalGet (nr (Int32.of_int (pick 4))))
    | 2 -> i (LocalSet (nr (Int32.of_int (pick 4))))
    | 3 -> i Drop
    | 4 -> i Return
    | 5 -> i Unreachable
    | _ -> i Nop
  in
  let rec build remaining labels =
    if remaining = 0 then leaf () else
    let nested () = build (remaining - 1) labels in
    match pick 10 with
    | 0 -> nop
    | 1 -> leaf ()
    | 2 | 3 ->
      let left = nested () in
      let right = nested () in
      left ^^ right
    | 4 ->
      let point = {file = "generated.mo"; line = pick 100; column = pick 20} in
      with_region {left = point; right = point} (nested ())
    | 5 -> block0 (nested ())
    | 6 -> loop0 (nested ())
    | 7 ->
      let thn = nested () in
      let els = nested () in
      if0 thn els
    | 8 ->
      let label = new_depth_label () in
      remember_depth label (build (remaining - 1) (label :: labels))
    | _ ->
      match labels with
      | [] -> leaf ()
      | _ -> branch_to_ (List.nth labels (pick (List.length labels)))
  in
  build 8 []

let generated_reference_equivalence () =
  let rec check seed =
    seed = 1_000 ||
    (materialize_raw_at 5l no_region (random_builder seed) =
       reference_raw 5l no_region (random_builder seed) &&
     to_instr_list (random_builder seed) =
       optimize (reference_raw 0l no_region (random_builder seed)) &&
     check (seed + 1))
  in
  check 0

let tests =
  [ "left-skewed 100,000-node append", left_skewed
  ; "right-skewed 100,000-node append", right_skewed
  ; "large concat", large_concat
  ; "large concat_map", large_concat_map
  ; "large terminating body", large_terminating_body
  ; "concat_mapi order", concat_mapi_order
  ; "table evaluation and emission order", table_orders
  ; "source regions", regions
  ; "if branch order", if_order
  ; "remember_depth timing", remember_before_body
  ; "loop backward branch", loop_branch
  ; "outward branch", outward_branch
  ; "raw setter", raw_setter
  ; "raw is_nop", raw_is_nop
  ; "nested optimization", nested_optimization
  ; "100,000 nested blocks", deep_blocks
  ; "100,000 nested if branches", deep_if_branches
  ; "region and depth restored for siblings", restored_region_and_depth
  ; "generated trees match recursive reference", generated_reference_equivalence
  ]

let () =
  List.iter
    (fun (name, test) ->
      if not (test ()) then failwith ("InstrList test failed: " ^ name))
    tests
