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
  test_identity ();
  test_all_index_spaces_and_names ();
  test_type_first_seen_order ();
  test_long_flat_body ();
  test_deeply_nested_body ()
