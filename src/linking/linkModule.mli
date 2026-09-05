(* The arguments are:
   - the base module
   - the name of the library to link
   - the module containing that library
*)
exception LinkError of string
exception TooLargeDataSegments of string

val link : Wasm_exts.CustomModule.extended_module -> string -> Wasm_exts.CustomModule.extended_module -> Wasm_exts.CustomModule.extended_module

module For_testing : sig
  val calculate_renaming :
    int32 -> int32 -> int32 ->
    (int32 * int32) list -> (int32 * int32) list ->
    (int32 -> int32) * (int32 -> int32)

  val remap_module_indices :
    ?type_index:(int32 -> int32) ->
    ?function_index:(int32 -> int32) ->
    ?global_index:(int32 -> int32) ->
    Wasm_exts.Ast.module_' ->
    Wasm_exts.Ast.module_'

  val rename_funcs_name_section :
    function_index:(int32 -> int32) ->
    Wasm_exts.CustomModule.name_section ->
    Wasm_exts.CustomModule.name_section
end
