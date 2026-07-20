(** Encode a module and its version-3 source map. This compatibility entry
    point always performs source-map collection; DWARF emission follows the
    compiler debug-info mode captured when encoding starts. *)
val encode : CustomModule.extended_module -> string * string

(** Encode only the Wasm into a freshly owned byte array. No source-map state,
    source content, or JSON is produced. *)
val encode_wasm_bytes : CustomModule.extended_module -> bytes

(** String compatibility wrapper around {!encode_wasm_bytes}. *)
val encode_wasm : CustomModule.extended_module -> string

module For_testing : sig
  type counters = {
    source_loads : int;
    map_serializations : int;
  }

  val observe : (unit -> 'a) -> 'a * counters
end

val promise_reference_slot : int Lib.Promise.t -> int
val allocate_reference_slot : unit -> int
