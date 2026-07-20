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

let () =
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
