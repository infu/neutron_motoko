open Wasm_exts

let empty_extended_module source_mapping_url = CustomModule.{
  module_ = Ast.empty_module;
  name = empty_name_section;
  dylink0 = [];
  candid = empty_candid_sections;
  motoko = empty_motoko_sections;
  enhanced_orthogonal_persistence = None;
  source_mapping_url;
  wasm_features = [];
  target_features = [];
}

let located file line it =
  let open Wasm.Source in
  let position = {file; line; column = 0} in
  {it; at = {left = position; right = position}}

let module_with_locations source_mapping_url =
  let open Ast in
  let type_ = located "first-source.mo" 1 (Types.FuncType ([], [])) in
  let ftype = located "first-source.mo" 1 0l in
  let body =
    [ located "first-source.mo" 2 Nop
    ; located "second-source.mo" 3 Nop ] in
  let func =
    located "second-source.mo" 3 {ftype; locals = []; body} in
  let extended = empty_extended_module source_mapping_url in
  CustomModule.{extended with
    module_ = {Ast.empty_module with types = [type_]; funcs = [func]}
  }

let with_debug_info enabled f =
  let previous = !(Mo_config.Flags.debug_info) in
  Fun.protect
    ~finally:(fun () -> Mo_config.Flags.debug_info := previous)
    (fun () ->
      Mo_config.Flags.debug_info := enabled;
      f ())

let contains haystack needle =
  let haystack_length = String.length haystack in
  let needle_length = String.length needle in
  let rec matches start offset =
    offset = needle_length ||
    (haystack.[start + offset] = needle.[offset] && matches start (offset + 1))
  in
  let rec search start =
    start + needle_length <= haystack_length &&
    (matches start 0 || search (start + 1))
  in
  search 0

let check_mode enabled module_ =
  with_debug_info enabled (fun () ->
    let source_map, mapped_wasm = CustomModuleEncode.encode module_ in
    let wasm_bytes = CustomModuleEncode.encode_wasm_bytes module_ in
    let wasm_string = CustomModuleEncode.encode_wasm module_ in
    assert (String.equal mapped_wasm wasm_string);
    assert (Bytes.equal wasm_bytes (Bytes.of_string wasm_string));
    let parsed_map = Yojson.Basic.from_string source_map in
    assert (Yojson.Basic.Util.member "version" parsed_map = `Int 3);
    source_map, wasm_string)

let module_with_body ?(locals = []) body =
  let open Ast in
  let extended = empty_extended_module None in
  let type_ = located "encoder.mo" 1 (Types.FuncType ([], [])) in
  let ftype = located "encoder.mo" 1 0l in
  let func = located "encoder.mo" 1 {ftype; locals; body} in
  CustomModule.{extended with
    module_ = {Ast.empty_module with types = [type_]; funcs = [func]}}

let read_u32 bytes cursor =
  let rec loop shift value =
    let byte = Char.code bytes.[!cursor] in
    incr cursor;
    let value = value lor ((byte land 0x7f) lsl shift) in
    if byte land 0x80 = 0 then value else loop (shift + 7) value
  in
  loop 0 0

(* Inspect the code payload independently of the production decoder, whose
   recursive instruction parser is not the subject of the encoder stress test. *)
let encoded_function bytes =
  let cursor = ref 8 in
  let rec find_code () =
    let id = Char.code bytes.[!cursor] in
    incr cursor;
    let length = read_u32 bytes cursor in
    if id <> 10 then begin cursor := !cursor + length; find_code () end
    else begin
      assert (read_u32 bytes cursor = 1);
      let body_size = read_u32 bytes cursor in
      let body_end = !cursor + body_size in
      let local_count = read_u32 bytes cursor in
      let rec read_locals remaining rev =
        if remaining = 0 then List.rev rev else
        let count = read_u32 bytes cursor in
        let ty = Char.code bytes.[!cursor] in
        incr cursor;
        read_locals (remaining - 1) ((ty, count) :: rev)
      in
      let locals = read_locals local_count [] in
      locals, !cursor, String.sub bytes !cursor (body_end - !cursor)
    end
  in
  find_code ()

let decode_map source_map =
  let open Yojson.Basic.Util in
  let json = Yojson.Basic.from_string source_map in
  let sources = json |> member "sources" |> to_list |> List.map to_string |> Array.of_list in
  let mappings = json |> member "mappings" |> to_string in
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let output = ref 0 and source = ref 0 and line = ref 0 and column = ref 0 in
  let decode segment =
    let cursor = ref 0 in
    let read () =
      let rec loop shift value =
        let digit = String.index alphabet segment.[!cursor] in
        incr cursor;
        let value = value lor ((digit land 31) lsl shift) in
        if digit land 32 <> 0 then loop (shift + 5) value
        else if value land 1 = 0 then value lsr 1 else -(value lsr 1)
      in
      loop 0 0
    in
    output := !output + read ();
    source := !source + read ();
    line := !line + read ();
    column := !column + read ();
    assert (!cursor = String.length segment);
    !output, sources.(!source), !line + 1, !column
  in
  if mappings = "" then [] else List.map decode (String.split_on_char ',' mappings)

(* This small recursive reference deliberately retains the old traversal order,
   including the synthetic Grouped nodes visited in debug mode. *)
let reference_body ~debug body =
  let open Ast in
  let open Wasm.Source in
  let open Dwarf5.Meta in
  let bytes = Buffer.create 64 in
  let mappings = ref [] in
  let op n = Buffer.add_char bytes (Char.chr n) in
  let rec instr e =
    if e.at <> no_region then
      mappings := (Buffer.length bytes, e.at.left.file, e.at.left.line, e.at.left.column) :: !mappings;
    match e.it with
    | Nop -> op 0x01
    | Unreachable -> op 0x00
    | Block (ValBlockType None, body) ->
      op 0x02; op 0x40; List.iter instr body; op 0x0b
    | Loop (ValBlockType None, body) ->
      op 0x03; op 0x40; List.iter instr body; op 0x0b
    | If (ValBlockType None, then_body, else_body) ->
      op 0x04; op 0x40; List.iter instr then_body;
      if else_body <> [] then op 0x05;
      List.iter instr else_body; op 0x0b
    | Meta _ when not debug -> ()
    | Meta (StatementDelimiter _) | Meta (Grouped []) -> ()
    | Meta (Grouped (late :: former)) ->
      instr {e with it = Meta (Grouped former)};
      instr {e with it = Meta late}
    | _ -> assert false
  in
  List.iter instr body;
  op 0x0b;
  Buffer.contents bytes, List.rev !mappings

let check_locals () =
  let open Types in
  let compress = CustomModuleEncode.For_testing.compress_locals in
  let reference locals =
    List.fold_right
      (fun t -> function
        | (t', n) :: rest when t = t' -> (t, n + 1) :: rest
        | rest -> (t, 1) :: rest)
      locals []
  in
  assert (compress [] = []);
  let random = Random.State.make [|0x10ca15|] in
  let types = [|I32Type; I64Type; F32Type; F64Type|] in
  for _ = 1 to 1_000 do
    let locals = List.init (Random.State.int random 300)
      (fun _ -> types.(Random.State.int random 4)) in
    assert (compress locals = reference locals)
  done;
  let same = List.init 100_000 (fun _ -> I32Type) in
  assert (compress same = [I32Type, 100_000]);
  let alternating = List.init 100_000 (fun i -> types.(i mod 4)) in
  let runs = compress alternating in
  assert (List.length runs = 100_000);
  List.iteri (fun i (ty, count) -> assert (ty = types.(i mod 4) && count = 1)) runs;
  with_debug_info false (fun () ->
    let same_bytes = CustomModuleEncode.encode_wasm (module_with_body ~locals:same []) in
    let same_runs, _, body = encoded_function same_bytes in
    assert (same_runs = [0x7f, 100_000]);
    assert (body = "\x0b");
    let alternating_bytes = CustomModuleEncode.encode_wasm (module_with_body ~locals:alternating []) in
    let alternating_runs, _, body = encoded_function alternating_bytes in
    assert (List.length alternating_runs = 100_000);
    List.iteri (fun i (ty, count) -> assert (ty = 0x7f - i mod 4 && count = 1)) alternating_runs;
    assert (body = "\x0b"))

let check_instruction_traversal () =
  let open Ast in
  let open Dwarf5.Meta in
  let random = Random.State.make [|0xecc0de|] in
  let next_line = ref 0 in
  let located it =
    incr next_line;
    located (if !next_line mod 2 = 0 then "first-source.mo" else "second-source.mo") !next_line it in
  let bt = ValBlockType None in
  let rec body depth =
    List.init (Random.State.int random 4) (fun _ ->
      let shape = if depth = 0 then Random.State.int random 3 else Random.State.int random 6 in
      located (match shape with
      | 0 -> Nop
      | 1 -> Unreachable
      | 2 ->
        let delimiter = StatementDelimiter Source.{file = "encoder.mo"; line = 1; column = 0} in
        Meta (Grouped [delimiter; Grouped [delimiter; delimiter]])
      | 3 -> Block (bt, body (depth - 1))
      | 4 -> Loop (bt, body (depth - 1))
      | _ -> If (bt, body (depth - 1), body (depth - 1))))
  in
  for _ = 1 to 100 do
    let body = body 4 in
    List.iter (fun debug ->
      let map, wasm = check_mode debug (module_with_body body) in
      let _, start, actual = encoded_function wasm in
      let expected, mappings = reference_body ~debug body in
      assert (actual = expected);
      let mappings = List.map (fun (offset, file, line, column) -> start + offset, file, line, column) mappings in
      assert (decode_map map = mappings)) [false; true]
  done;
  with_debug_info false (fun () ->
    let bare it = Wasm.Source.(it @@ no_region) in
    let flat = List.init 100_000 (fun _ -> bare Nop) in
    let _, _, flat_bytes = encoded_function (CustomModuleEncode.encode_wasm (module_with_body flat)) in
    assert (flat_bytes = String.make 100_000 '\x01' ^ "\x0b");
    let rec nested count body expected_length =
      if count = 0 then body, expected_length else
      match count mod 3 with
      | 0 -> nested (count - 1) [bare (Block (bt, body))] (expected_length + 3)
      | 1 -> nested (count - 1) [bare (Loop (bt, body))] (expected_length + 3)
      | _ -> nested (count - 1) [bare (If (bt, body, [bare Nop]))] (expected_length + 5)
    in
    let deep, length = nested 100_000 [bare Nop] 2 in
    let _, _, deep_bytes = encoded_function (CustomModuleEncode.encode_wasm (module_with_body deep)) in
    assert (String.length deep_bytes = length))

let () =
  check_locals ();
  check_instruction_traversal ();
  let module_ = module_with_locations (Some "kernel.wasm.map") in
  let open CustomModuleEncode.For_testing in
  let _, no_map_counters =
    observe
      (fun () -> CustomModuleEncode.encode_wasm_bytes module_) in
  assert (no_map_counters.source_loads = 0);
  assert (no_map_counters.map_serializations = 0);
  let _, map_counters =
    observe
      (fun () -> CustomModuleEncode.encode module_) in
  assert (map_counters.source_loads = 2);
  assert (map_counters.map_serializations = 1);

  let map_without_debug, wasm_without_debug = check_mode false module_ in
  let map_with_debug, wasm_with_debug = check_mode true module_ in
  assert (String.equal map_without_debug map_with_debug);
  assert (not (String.equal wasm_without_debug wasm_with_debug));

  let map_json = Yojson.Basic.from_string map_without_debug in
  let sources =
    Yojson.Basic.Util.(map_json |> member "sources" |> to_list |> List.map to_string) in
  let sources_content =
    Yojson.Basic.Util.(map_json |> member "sourcesContent" |> to_list) in
  assert (sources = ["first-source.mo"; "second-source.mo"]);
  assert (List.length sources_content = List.length sources);

  let bytes =
    with_debug_info false (fun () -> CustomModuleEncode.encode_wasm_bytes module_) in
  assert (Bytes.length bytes >= 8);
  assert (Bytes.sub_string bytes 0 8 = "\x00asm\x01\x00\x00\x00");

  let original_first_byte = Bytes.get bytes 0 in
  Bytes.set bytes 0 '\xff';
  let later =
    with_debug_info false (fun () -> CustomModuleEncode.encode_wasm_bytes module_) in
  assert (Bytes.get later 0 = original_first_byte);

  let later_string = Bytes.to_string later in
  ignore (CustomModuleDecode.decode "encoder-test" later_string);
  assert (contains later_string "sourceMappingURL");
  assert (contains later_string "kernel.wasm.map")
