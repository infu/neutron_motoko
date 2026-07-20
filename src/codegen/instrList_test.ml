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
  ]

let () =
  List.iter
    (fun (name, test) ->
      if not (test ()) then failwith ("InstrList test failed: " ^ name))
    tests
