open Wasm_exts.Ast
open Wasm.Source
open Wasm_exts.CustomModule

module Remap = Linking.LinkModule.For_testing

let region =
  let left = { file = "linkModule-remapping"; line = 1; column = 2 } in
  let right = { file = "linkModule-remapping"; line = 3; column = 4 } in
  { left; right }

let var index = index @@ region
let instr instruction = instruction @@ region
let const instructions = instructions @@ region

let test_module type_offset function_offset global_offset =
  let add offset index = Int32.add offset index in
  let type_var index = var (add type_offset index) in
  let function_var index = var (add function_offset index) in
  let global_var index = var (add global_offset index) in
  let body = [
    instr (Call (function_var 1l));
    instr (Block (VarBlockType (type_var 2l), [
      instr (Loop (ValBlockType None, [
        instr (If (VarBlockType (type_var 3l),
          [instr (CallIndirect (var 70l, type_var 4l))],
          [instr (GlobalGet (global_var 5l));
           instr (GlobalSet (global_var 6l))]
        ))
      ]))
    ]));
  ] in
  let func = {
    ftype = type_var 7l;
    locals = [];
    body;
  } @@ region in
  let import = {
    module_name = Lib.Utf8.decode "module";
    item_name = Lib.Utf8.decode "function";
    idesc = FuncImport (type_var 8l) @@ region;
  } @@ region in
  let global = {
    gtype = Wasm_exts.Types.GlobalType
      (Wasm_exts.Types.I32Type, Wasm_exts.Types.Immutable);
    value = const [instr (GlobalGet (global_var 9l))];
  } @@ region in
  let elem = {
    index = var 71l;
    offset = const [instr (GlobalGet (global_var 10l))];
    init = [function_var 11l];
  } @@ region in
  let active_data = {
    dinit = "active";
    dmode = Active {
      index = var 72l;
      offset = const [instr (GlobalGet (global_var 12l))];
    } @@ region;
  } @@ region in
  let passive_data = {
    dinit = "passive";
    dmode = Passive @@ region;
  } @@ region in
  let export name edesc = {
    name = Lib.Utf8.decode name;
    edesc = edesc @@ region;
  } @@ region in
  { empty_module with
    funcs = [func];
    imports = [import];
    globals = [global];
    elems = [elem];
    datas = [active_data; passive_data];
    exports = [
      export "function" (FuncExport (function_var 13l));
      export "global" (GlobalExport (global_var 14l));
      export "table" (TableExport (var 73l));
      export "memory" (MemoryExport (var 74l));
    ];
    start = Some (function_var 15l);
  }

let check name condition =
  if not condition then failwith ("linker remapping test failed: " ^ name)

(* Preserve the original list-scanning implementation as an independent oracle
   for the defined-index fast paths, including recursive import resolution. *)
let original_calculate_renaming
    n_imports1 n_things1 n_imports2 resolved12 resolved21 =
  let open Int32 in
  let n_imports1' = sub n_imports1 (Lib.List32.length resolved12) in
  let n_imports2' = sub n_imports2 (Lib.List32.length resolved21) in
  let rec fun1 i =
    let rec go skipped = function
      | (imp, exp) :: is ->
        if i < imp then sub i skipped
        else if i = imp then fun2 exp
        else go (add skipped 1l) is
      | [] ->
        if i < n_imports1
        then sub i skipped
        else sub (add i n_imports2') skipped
    in go 0l resolved12
  and fun2 i =
    let rec go skipped = function
      | (imp, exp) :: is ->
        if i < imp then sub (add i n_imports1') skipped
        else if i = imp then fun1 exp
        else go (add skipped 1l) is
      | [] ->
        if i < n_imports2
        then sub (add i n_imports1') skipped
        else sub (add (add i n_imports1') n_things1) skipped
    in go 0l resolved21
  in
  fun1, fun2

let compare_renamings
    n_imports1 n_things1 n_imports2 n_things2 resolved12 resolved21 =
  let arguments calculate =
    calculate (Int32.of_int n_imports1) (Int32.of_int n_things1)
      (Int32.of_int n_imports2) resolved12 resolved21
  in
  let actual1, actual2 = arguments Remap.calculate_renaming in
  let expected1, expected2 = arguments original_calculate_renaming in
  let compare side actual expected last =
    (* Include the first index after each module as well: for a module without
       definitions this still exercises the exact import/definition boundary. *)
    for index = 0 to last do
      let index = Int32.of_int index in
      if actual index <> expected index then
        failwith (Printf.sprintf
          "linker renaming mismatch: module %d, index %ld" side index)
    done
  in
  compare 1 actual1 expected1 (n_imports1 + n_things1);
  compare 2 actual2 expected2 (n_imports2 + n_things2)

let every_resolution n_imports n_targets visit =
  let rec choose index reversed =
    if index = n_imports then visit (List.rev reversed)
    else begin
      choose (index + 1) reversed;
      for target = 0 to n_targets - 1 do
        choose (index + 1)
          ((Int32.of_int index, Int32.of_int target) :: reversed)
      done
    end
  in
  choose 0 []

let terminating_resolutions resolved12 resolved21 =
  (* Cyclic import aliases do not terminate in the original linker either;
     enumerate and exclude them without changing the linker's behavior. *)
  let limit = List.length resolved12 + List.length resolved21 + 1 in
  let rec terminates remaining first index =
    if remaining = 0 then false
    else
      match List.assoc_opt index (if first then resolved12 else resolved21) with
      | None -> true
      | Some target -> terminates (remaining - 1) (not first) target
  in
  List.for_all (fun (index, _) -> terminates limit true index) resolved12 &&
  List.for_all (fun (index, _) -> terminates limit false index) resolved21

let test_exhaustive_renaming () =
  let compared = ref 0 in
  let cyclic = ref 0 in
  for n_imports1 = 0 to 3 do
    for n_imports2 = 0 to 3 do
      for n_things1 = 0 to 2 do
        for n_things2 = 0 to 2 do
          every_resolution n_imports1 (n_imports2 + n_things2)
            (fun resolved12 ->
              every_resolution n_imports2 (n_imports1 + n_things1)
                (fun resolved21 ->
                  if terminating_resolutions resolved12 resolved21 then begin
                    incr compared;
                    compare_renamings n_imports1 n_things1 n_imports2 n_things2
                      resolved12 resolved21
                  end else incr cyclic))
        done
      done
    done
  done;
  check "terminating import graphs exercised" (!compared > 100_000);
  check "cyclic import graphs excluded" (!cyclic > 0);
  Printf.printf "linker renaming: %d terminating import graphs compared\n%!"
    !compared

let test_renaming_boundaries () =
  (* These resolutions include a chain through both modules, gaps, a direct
     reference to an unresolved import, and both first defined indices. *)
  let first, second = Remap.calculate_renaming 3l 2l 3l
      [0l, 1l; 2l, 0l] [1l, 2l; 2l, 3l] in
  check "cross-import chain and first module boundary"
    (List.map first [0l; 1l; 2l; 3l; 4l] = [1l; 0l; 1l; 2l; 3l]);
  check "unresolved import and second module boundary"
    (List.map second [0l; 1l; 2l; 3l; 4l] = [1l; 1l; 2l; 4l; 5l]);
  let actual1, actual2 = Remap.calculate_renaming
      2l Int32.max_int 3l [1l, 3l] [0l, 2l; 2l, 1l] in
  let expected1, expected2 = original_calculate_renaming
      2l Int32.max_int 3l [1l, 3l] [0l, 2l; 2l, 1l] in
  List.iter (fun index ->
    check "Int32 first-module arithmetic"
      (actual1 index = expected1 index);
    check "Int32 second-module arithmetic"
      (actual2 index = expected2 index)
  ) [0l; 1l; 2l; 3l; Int32.pred Int32.max_int; Int32.max_int]

let test_large_renaming () =
  let resolved12 = List.init 256 (fun index -> index)
    |> List.filter_map (fun index ->
      if index mod 3 <> 0 then None
      else Some (Int32.of_int index, Int32.of_int (384 + index * 13))) in
  let resolved21 = List.init 384 (fun index -> index)
    |> List.filter_map (fun index ->
      if index mod 5 <> 0 then None
      else Some (Int32.of_int index, Int32.of_int (256 + index * 37))) in
  compare_renamings 256 20_000 384 4_000 resolved12 resolved21

let test_identity () =
  let module_ = test_module 0l 0l 0l in
  check "identity" (Remap.remap_module_indices module_ = module_)

let test_all_index_spaces_and_names () =
  let add offset index = Int32.add offset index in
  let actual =
    Remap.remap_module_indices
      ~type_index:(add 100l)
      ~function_index:(add 200l)
      ~global_index:(add 300l)
      (test_module 0l 0l 0l)
  in
  check "all index spaces" (actual = test_module 100l 200l 300l);
  let names = {
    empty_name_section with
    function_names = [1l, "function"];
    locals_names = [2l, [0l, "local"]];
    label_names = [3l, [0l, "label"]];
  } in
  let actual_names =
    Remap.rename_funcs_name_section ~function_index:(add 200l) names
  in
  check "function and local names" (actual_names = {
    names with
    function_names = [201l, "function"];
    locals_names = [202l, [0l, "local"]];
  })

let test_type_first_seen_order () =
  let reversed = ref [] in
  let observe index =
    reversed := index :: !reversed;
    index
  in
  ignore (Remap.remap_module_indices ~type_index:observe
    (test_module 0l 0l 0l));
  check "type first-seen order"
    (List.rev !reversed = [8l; 4l; 3l; 2l; 7l])

let module_with_body body =
  let func = {
    ftype = var 0l;
    locals = [];
    body;
  } @@ region in
  { empty_module with funcs = [func] }

let body module_ =
  match module_.funcs with
  | [{ it = { body; _ }; _ }] -> body
  | _ -> assert false

let test_long_flat_body () =
  let count = 100_000 in
  let rec make_calls index reversed =
    if index < 0 then reversed
    else
      make_calls (index - 1)
        (instr (Call (var (Int32.of_int index))) :: reversed)
  in
  let module_ = module_with_body (make_calls (count - 1) []) in
  let remapped =
    body (Remap.remap_module_indices ~function_index:Int32.succ module_)
  in
  let rec verify expected remaining =
    match remaining with
    | [] -> expected = count
    | { it = Call function_index; at } :: remaining ->
      if at <> region ||
         function_index.at <> region ||
         function_index.it <> Int32.of_int (expected + 1)
      then false
      else verify (expected + 1) remaining
    | _ -> false
  in
  check "long flat body" (verify 0 remapped)

let test_deeply_nested_body () =
  let depth = 100_000 in
  let leaf = instr (Call (var 1l)) in
  let rec nest remaining instruction =
    if remaining = 0 then instruction
    else
      nest (remaining - 1)
        (instr (Block (VarBlockType (var 2l), [instruction])))
  in
  let module_ = module_with_body [nest depth leaf] in
  let remapped = Remap.remap_module_indices
      ~type_index:Int32.succ ~function_index:Int32.succ module_ in
  let result = match body remapped with
    | [instruction] ->
      let rec verify remaining instruction =
        if instruction.at <> region then false
        else if remaining = 0 then
          match instruction.it with
          | Call function_index ->
            function_index.it = 2l && function_index.at = region
          | _ -> false
        else
          match instruction.it with
          | Block (VarBlockType type_index, [nested]) ->
            if type_index.it <> 3l || type_index.at <> region
            then false
            else verify (remaining - 1) nested
          | _ -> false
      in
      verify depth instruction
    | _ -> false
  in
  check "deeply nested body" result

let () =
  test_exhaustive_renaming ();
  test_renaming_boundaries ();
  test_large_renaming ();
  test_identity ();
  test_all_index_spaces_and_names ();
  test_type_first_seen_order ();
  test_long_flat_body ();
  test_deeply_nested_body ()
