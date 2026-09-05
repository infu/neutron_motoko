open Wasm_exts
open Source
open Mo_config

module Js = Js_of_ocaml.Js
module Sys_js = Js_of_ocaml.Sys_js

(** Extra flags from the [moc] CLI *)
let moc_args = Mo_args.inclusion_args
  @ Mo_args.warning_args
  @ Mo_args.error_args
  @ Mo_args.ai_args
  @ Mo_args.persistent_actors_args
  @ Mo_args.migration_args

let position_of_pos pos =
  object%js
    (* The LSP spec requires zero-based positions *)
    val line = if pos.line > 0 then pos.line - 1 else 0
    val character = pos.column
  end

let range_of_region at =
  object%js
    val start = position_of_pos at.left
    val _end = position_of_pos at.right
  end

let diagnostics_of_msg (msg : Diag.message) =
  Diag.(object%js
    val source = Js.string msg.at.left.file
    val severity = if Diag.is_treated_as_error msg then 1 else 2
    val range = range_of_region msg.at
    val code = Js.string msg.code
    val category = Js.string msg.cat
    val message = Js.string msg.text
  end)

let diagnostics_of_msgs (msgs : Diag.message list) =
  match msgs with
  | [] -> [||]
  | first :: rest ->
    let diagnostics = Array.make (List.length msgs) (diagnostics_of_msg first) in
    let rec fill index = function
      | [] -> diagnostics
      | msg :: remaining ->
        diagnostics.(index) <- diagnostics_of_msg msg;
        fill (index + 1) remaining
    in
    fill 1 rest

let rec js_of_sexpr (sexpr : Wasm.Sexpr.sexpr) : Js.Unsafe.any =
  (* generate a JSON-serializable value tree from an s-expression *)
  match sexpr with
| Wasm.Sexpr.Node (head, inner) ->
  Js.Unsafe.coerce (object%js
    val name = Js.string head
    val args = inner |> List.map js_of_sexpr |> Array.of_list |> Js.array |> Js.some
  end)
| Wasm.Sexpr.Atom s ->
  Js.Unsafe.coerce (Js.string s)

let js_result (result : 'a Diag.result) (wrap_code: 'a -> 'b) =
  match result with
  | Ok (code, msgs) ->
     object%js
       val diagnostics = Js.array (diagnostics_of_msgs msgs)
       val code = wrap_code code
     end
  | Error msgs ->
     object%js
       val diagnostics = Js.array (diagnostics_of_msgs msgs)
       val code = Js.null
     end

let js_version = Js.string Source_id.id

let js_check source =
  Mo_types.Cons.session (fun _ ->
    js_result
      (Pipeline.check_files ~enable_recovery:true [Js.to_string source])
      (fun _ -> Js.null))

let parse_extra_flags tokens =
  let argv = Array.append [|"mocjs"|] tokens in
  let current = ref 0 in
  let usage = "moc extra flags" in
  Arg.parse_argv ~current argv moc_args (fun _ -> ()) usage 

let js_set_extra_flags flags =
  let tokens = flags |> Js.to_array |> Array.map Js.to_string in
  parse_extra_flags tokens

let js_set_run_step_limit limit =
  Mo_interpreter.Interpret.step_limit := limit

let js_run list source =
  Mo_types.Cons.session (fun _ ->
    let list = Js.to_array list |> Array.to_list |> List.map Js.to_string in
    match Pipeline.run_stdin_from_file list (Js.to_string source) with
    | Some v ->
      object%js
        (* TODO: val value = js_value v *)
        val error = Js.null
      end
    | None ->
      object%js
        val error = Js.some (object%js
          (* empty for compatibility with returning a message, type, location, etc. in the future *)
        end)
      end)

let js_candid source =
  Mo_types.Cons.session (fun _ ->
    js_result (Pipeline.generate_idl [Js.to_string source])
      (fun prog ->
        let open Idllib in
        let module WithComments = Arrange_idl.Make(struct let trivia = Some prog.note.Syntax.trivia end) in
        let code = WithComments.string_of_prog prog in
        Js.some (Js.string code)))

let js_stable_compatible pre post =
  js_result (Pipeline.stable_compatible (Js.to_string pre) (Js.to_string post)) (fun _ -> Js.null)

let js_compile_wasm mode source =
  let source = Js.to_string source in
  let mode =
    match Js.to_string mode with
    | "wasi" -> Flags.WASIMode
    | "ic" -> Flags.ICMode
    | _ -> raise (Invalid_argument "js_compile_with: Unexpected mode")
  in
  Mo_types.Cons.session (fun _ -> js_result (Pipeline.compile_files mode true [source])
    (fun (idl_prog, m) ->
      let open CustomModule in
      let sig_ = match m.motoko.stable_types with
        | Some (_, txt) -> Js.some (Js.string txt)
        | _ -> Js.null in
      let candid = Idllib.Arrange_idl.string_of_prog idl_prog in
      let wasm = CustomModuleEncode.encode_wasm_bytes m in
      let borrowed = Js_of_ocaml.Typed_array.Bytes.to_uint8Array wasm in
      let constructor = Js.Unsafe.global##._Uint8Array in
      (* Typed_array.Bytes may alias the OCaml bytes. The constructor overload
         taking another typed array performs one bulk copy into caller-owned
         storage, so transferring it cannot detach compiler state. *)
      let code = Js.Unsafe.new_obj constructor [|Js.Unsafe.inject borrowed|] in
      Js.some (object%js
        val wasm = code
        val candid = Js.string candid
        val stable = sig_
      end)))

let js_parse_candid s =
  let parse_result = Idllib.Pipeline.parse_string (Js.to_string s) in
  js_result parse_result (fun (prog, _) ->
    Js.some (js_of_sexpr (Idllib.Arrange_idl.prog prog)))

let js_parse_motoko enable_recovery s =
  let main_file = "" in
  let parse_fn = if Js.Opt.get enable_recovery (fun () -> false)
    then Pipeline.parse_string_with_recovery
    else Pipeline.parse_string
  in
  let parse_result = parse_fn main_file (Js.to_string s) in
  js_result parse_result (fun (prog, _) ->
    let open Mo_def in
    let module Arrange = Astjs.Make (struct
      let include_sources = true
      let include_type_rep = Arrange.Without_type_rep
      let include_types = false
      let include_docs = Some prog.note.Syntax.trivia
      let include_parenthetical = false
      let main_file = Some main_file
    end)
    in Js.some (Arrange.prog_js prog))

let js_parse_motoko_with_deps enable_recovery path s =
  let main_file = Js.to_string path in
  let s = Js.to_string s in
  let parse_fn = if Js.Opt.get enable_recovery (fun () -> false)
    then Pipeline.parse_string_with_recovery
    else Pipeline.parse_string
  in
  let prog_and_deps_result =
    let open Diag.Syntax in
    let* prog, _ = parse_fn main_file s in
    let* deps =
      Pipeline.ResolveImport.resolve (Pipeline.resolve_flags ~is_main:true ~base:main_file None) prog main_file
    in
    Diag.return (prog, deps)
  in
  js_result prog_and_deps_result (fun (prog, deps) ->
    let open Mo_def in
    let module Arrange = Astjs.Make (struct
      let include_sources = true
      let include_type_rep = Arrange.Without_type_rep
      let include_types = false
      let include_docs = Some prog.note.Syntax.trivia
      let include_parenthetical = false
      let main_file = Some main_file
    end) in
    Js.some (
      object%js
        val ast = Arrange.prog_js prog
        val immediateImports =
          deps
          |> List.map (fun dep -> Js.string (Pipeline.resolved_import_name dep))
          |> Array.of_list
          |> Js.array
      end))

module Compact_inspect = struct
  open Mo_def
  open Syntax

  type work =
    | Exp of exp
    | Dec of dec
    | DecField of dec_field
    | ExpField of exp_field
    | Case of case
    | Pat of pat
    | PatField of pat_field

  type result = {
    imports : string list;
    has_actor_url : bool;
    dot_members : string list;
    pattern_fields : string list;
  }

  let prepend f xs rest =
    List.rev_append (List.rev_map f xs) rest

  let inspect (prog : prog) =
    let imports = ref [] in
    let has_actor_url = ref false in
    let dot_members = ref [] in
    let seen_dot_members = Hashtbl.create 16 in
    let pattern_fields = ref [] in
    let seen_pattern_fields = Hashtbl.create 16 in
    let add_dot_member member =
      if not (Hashtbl.mem seen_dot_members member) then begin
        Hashtbl.add seen_dot_members member ();
        dot_members := member :: !dot_members
      end
    in
    let add_pattern_field field =
      if not (Hashtbl.mem seen_pattern_fields field) then begin
        Hashtbl.add seen_pattern_fields field ();
        pattern_fields := field :: !pattern_fields
      end
    in
    let prepend_shared_pattern sort rest =
      match sort.it with
      | Mo_types.Type.Local -> rest
      | Mo_types.Type.Shared (_, pattern) -> Pat pattern :: rest
    in
    let rec loop = function
      | [] ->
        {
          imports = List.rev !imports;
          has_actor_url = !has_actor_url;
          dot_members = List.rev !dot_members;
          pattern_fields = List.rev !pattern_fields;
        }
      | Exp e :: rest ->
        begin match e.it with
        | HoleE (_, inner) -> loop (Exp !inner :: rest)
        | PrimE _ | VarE _ | LitE _ | ImplicitLibE _ -> loop rest
        | ImportE (path, _) ->
          imports := path :: !imports;
          loop rest
        | ActorUrlE inner ->
          has_actor_url := true;
          loop (Exp inner :: rest)
        | UnE (_, _, inner)
        | ShowE (_, inner)
        | FromCandidE inner
        | ProjE (inner, _)
        | OptE inner
        | DoOptE inner
        | BangE inner
        | TagE (_, inner)
        | NotE inner
        | AssertE (_, inner)
        | LabelE (_, _, inner)
        | BreakE (_, _, inner)
        | RetE inner
        | AwaitE (_, inner)
        | ThrowE inner
        | AnnotE (inner, _)
        | DebugE inner
        | IgnoreE inner ->
          loop (Exp inner :: rest)
        | DotE (inner, member, _) ->
          add_dot_member member.it;
          loop (Exp inner :: rest)
        | BinE (_, left, _, right)
        | RelE (_, left, _, right)
        | IdxE (left, right)
        | AssignE (left, right)
        | AndE (left, right)
        | OrE (left, right)
        | NullCoalesceE (left, right)
        | WhileE (left, right, _) ->
          loop (Exp left :: Exp right :: rest)
        | ToCandidE es | TupE es | ArrayE (_, es) ->
          loop (prepend (fun child -> Exp child) es rest)
        | ObjBlockE (_, _, _, fields) ->
          (* js_parse_motoko deliberately omits the parenthetical expression. *)
          loop (prepend (fun field -> DecField field) fields rest)
        | ObjE (bases, fields) ->
          let rest = prepend (fun field -> ExpField field) fields rest in
          loop (prepend (fun base -> Exp base) bases rest)
        | FuncE (_, sort, _, pattern, _, _, body) ->
          loop (prepend_shared_pattern sort (Pat pattern :: Exp body :: rest))
        | CallE (_, callee, _, (_, argument)) ->
          (* Match the full-AST API's include_parenthetical = false behavior. *)
          loop (Exp callee :: Exp !argument :: rest)
        | BlockE declarations ->
          loop (prepend (fun declaration -> Dec declaration) declarations rest)
        | IfE (condition, yes, no) ->
          loop (Exp condition :: Exp yes :: Exp no :: rest)
        | SwitchE (scrutinee, cases) ->
          loop (Exp scrutinee :: prepend (fun case -> Case case) cases rest)
        | LoopE (body, None, _) ->
          loop (Exp body :: rest)
        | LoopE (body, Some condition, _) ->
          loop (Exp body :: Exp condition :: rest)
        | ForE (pattern, iterator, body, _) ->
          loop (Pat pattern :: Exp iterator :: Exp body :: rest)
        | AsyncE (_, _, _, body) ->
          loop (Exp body :: rest)
        | TryE (body, cases, finally) ->
          let rest = match finally with None -> rest | Some e -> Exp e :: rest in
          loop (Exp body :: prepend (fun case -> Case case) cases rest)
        end
      | Dec declaration :: rest ->
        begin match declaration.it with
        | ExpD e | VarD (_, e) -> loop (Exp e :: rest)
        | LetD (pattern, e, None) -> loop (Pat pattern :: Exp e :: rest)
        | LetD (pattern, e, Some fail) ->
          loop (Pat pattern :: Exp e :: Exp fail :: rest)
        | TypD _ -> loop rest
        | ClassD (_, sort, _, _, _, pattern, _, _, fields) ->
          (* Class parenthetical expressions are omitted by js_parse_motoko. *)
          loop (prepend_shared_pattern sort
            (Pat pattern :: prepend (fun field -> DecField field) fields rest))
        | MixinD (_, pattern, fields) ->
          loop (Pat pattern :: prepend (fun field -> DecField field) fields rest)
        | IncludeD (_, _, e, _) -> loop (Exp e :: rest)
        end
      | DecField field :: rest -> loop (Dec field.it.dec :: rest)
      | ExpField field :: rest -> loop (Exp field.it.exp :: rest)
      | Case case :: rest -> loop (Pat case.it.pat :: Exp case.it.exp :: rest)
      | Pat pattern :: rest ->
        begin match pattern.it with
        | WildP | VarP _ | LitP _ | SignP _ -> loop rest
        | TupP patterns -> loop (prepend (fun child -> Pat child) patterns rest)
        | ObjP fields -> loop (prepend (fun field -> PatField field) fields rest)
        | OptP child | TagP (_, child) | AnnotP (child, _) | ParP child ->
          loop (Pat child :: rest)
        | AltP (left, right) | AndP (left, right) ->
          loop (Pat left :: Pat right :: rest)
        end
      | PatField field :: rest ->
        begin match field.it with
        | ValPF (name, pattern) ->
          (* Match Arrange's ValPF, not local bindings or type fields. *)
          add_pattern_field name.it;
          loop (Pat pattern :: rest)
        | TypPF _ -> loop rest
        end
    in
    loop (prepend (fun declaration -> Dec declaration) prog.it [])
end

let js_inspect_motoko path s =
  let path = Js.to_string path in
  let parse_result = Pipeline.parse_string path (Js.to_string s) in
  js_result parse_result (fun (prog, _) ->
    let inspection = Compact_inspect.inspect prog in
    let open Compact_inspect in
    let strings xs =
      match xs with
      | [] -> Js.array [||]
      | first :: rest ->
        let values = Array.make (List.length xs) (Js.string first) in
        let rec fill index = function
          | [] -> Js.array values
          | value :: remaining ->
            values.(index) <- Js.string value;
            fill (index + 1) remaining
        in
        fill 1 rest
    in
    Js.some (object%js
      val immediateImports = strings inspection.imports
      val hasActorUrl = Js.bool inspection.has_actor_url
      val dotMembers = strings inspection.dot_members
      val patternFields = strings inspection.pattern_fields
    end))

module Map_conversion (Map : Map.S) = struct
  let from_js
    (type data)
    (map : _ Js.t)
    (from_key : 'k Js.t -> Map.key)
    (from_data : 'd Js.t -> data)
    : data Map.t
    =
    let result = ref Map.empty in
    let callback =
      (* [forEach] in JS gives value, key, and map, in this order. *)
      Js.wrap_callback (fun v k _m ->
          let k = from_key k in
          let v = from_data v in
          result := Map.add k v !result)
    in
    ignore (Js.Unsafe.meth_call map "forEach" [|Js.Unsafe.inject callback|]);
    !result

  let to_js
    (type data)
    (map : data Map.t)
    (from_key : Map.key -> Js.Unsafe.top Js.t)
    (from_data : data -> Js.Unsafe.top Js.t)
    : _ Js.t
    =
    let js_map = Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "Map") [||] in
    Map.iter
      (fun k v ->
         ignore (Js.Unsafe.meth_call js_map "set" [| from_key k; from_data v |]))
      map;
    js_map

  let from_ocaml = to_js
  let to_ocaml = from_js
end

(* Scope cache conversion between JS Maps and OCaml Type.Env.
   The data of the map has TypeScript [type Scope = unknown], and such
   scopes are always produced by the compiler. The language server takes
   scopes as outputs and gives them as inputs without touching them at
   all. Hence, the use of [Obj.magic] is legitimate here. *)
module ScopeCacheConversion = Map_conversion (Mo_types.Type.Env)

let scope_cache_from_js js_cache =
  ScopeCacheConversion.from_js js_cache Js.to_string Obj.magic

let scope_cache_to_js cache =
  ScopeCacheConversion.to_js
    cache
    (fun k -> Js.Unsafe.inject (Js.string k))
    Obj.magic

let load_progs_cached_js
    ~enable_type_recovery
    parse_fn
    paths
    scope_cache =
  Mo_types.Cons.session (fun () ->
    Pipeline.load_progs_cached ~enable_type_recovery
      parse_fn paths Pipeline.initial_stat_env scope_cache)

let js_parse_motoko_typed_with_scope_cache_impl enable_recovery paths scope_cache =
  let paths = paths |> Js.to_array |> Array.to_list |> List.map Js.to_string in
  let scope_cache =
    Js.Opt.case scope_cache
      (fun () -> Mo_types.Type.Env.empty)
      scope_cache_from_js
  in
  let recovery_enabled = Js.Opt.get enable_recovery (fun () -> false) in
  let parse_fn = if recovery_enabled
    then Pipeline.parse_file_with_recovery
    else Pipeline.parse_file
  in
  let load_result =
    load_progs_cached_js ~enable_type_recovery:recovery_enabled
      parse_fn paths scope_cache
  in
  match load_result with
  (* senv: accumulated scope from prelude and all transitive imports *)
  | Ok ((_libs, progs, senv, scope_cache), msgs) ->
    let progs =
      progs |> List.map (fun (prog, immediate_imports, sscope) ->
        let open Mo_def in
        let module Arrange = Astjs.Make (struct
          let include_sources = true
          let include_type_rep = Arrange.With_type_rep (Some sscope.Mo_frontend.Scope.fld_src_env)
          let include_types = true
          let include_docs = Some prog.note.Syntax.trivia
          let include_parenthetical = false
          let main_file = Some prog.at.left.file
        end)
        in
        ( Arrange.prog_js prog
        (* , js_of_sexpr (Arrange_sources_types.typ typ) *)
        , immediate_imports |> List.map Js.string |> Array.of_list |> Js.array
        , senv )
      ) |> Array.of_list
    in
    Ok ((progs, scope_cache_to_js scope_cache), msgs)
  | Error msgs -> Error msgs

let js_parse_motoko_typed_with_scope_cache enable_recovery paths scope_cache =
  let result =
    js_parse_motoko_typed_with_scope_cache_impl enable_recovery paths (Js.Opt.return scope_cache)
  in
  js_result result (fun (progs, scope_cache) ->
    let progs =
      progs |> Array.map (fun (ast, immediate_imports, senv) ->
        object%js
          val ast = ast
          (* val typ = typ *)
          val immediateImports = immediate_imports
          val scope = Js.Unsafe.inject senv
        end)
      |> Js.array
    in
    Js.some (Js.array [| Js.Unsafe.inject progs; Js.Unsafe.inject scope_cache |]))

let js_parse_motoko_typed paths =
  let result = js_parse_motoko_typed_with_scope_cache_impl Js.Opt.empty paths Js.Opt.empty in
  js_result result (fun (progs, _scope_cache) ->
    let progs =
      progs |> Array.map (fun (ast, _immediate_imports, senv) ->
        object%js
          val ast = ast
          (* val typ = typ *)
          val scope = Js.Unsafe.inject senv
        end)
      |> Js.array
    in
    Js.some (Js.Unsafe.inject progs))

let js_check_with_scope_cache source scope_cache =
  let load_result =
    load_progs_cached_js
      ~enable_type_recovery:true
      Pipeline.parse_file_with_recovery
      [Js.to_string source]
      (scope_cache_from_js scope_cache)
  in
  let msgs, scope_cache_js = match load_result with
    | Ok ((_libs, _progs, _senv, scope_cache), msgs) ->
      msgs, Js.some (scope_cache_to_js scope_cache)
    | Error msgs ->
      msgs, Js.null
  in
  object%js
    val diagnostics = Js.array (diagnostics_of_msgs msgs)
    val scopeCache = scope_cache_js
  end

let js_save_file filename content =
  let filename = Js.to_string filename in
  let content = Js.to_string content in
  try Sys_js.create_file ~name:filename ~content:content
  with _ -> Sys_js.update_file ~name:filename ~content:content

let js_remove_file filename = Sys.remove (Js.to_string filename)
let js_rename_file oldpath newpath = Sys.rename (Js.to_string oldpath) (Js.to_string newpath)
let js_read_file path = Sys_js.read_file ~name:(Js.to_string path) |> Js.string
let js_read_dir path = Sys.readdir (Js.to_string path) |> Array.map Js.string |> Js.array
let stdout_buffer = Buffer.create(1000)
let stderr_buffer = Buffer.create(1000)

let clear_output_buffers () =
  Buffer.clear stdout_buffer;
  Buffer.clear stderr_buffer

let discard_output f =
  clear_output_buffers ();
  Fun.protect ~finally:clear_output_buffers f

let wrap_output f =
  clear_output_buffers ();
  Fun.protect ~finally:clear_output_buffers (fun () ->
    let result = f () in
    let stdout_result = Buffer.contents stdout_buffer in
    let stderr_result = Buffer.contents stderr_buffer in
    object%js
      val stdout = Js.bytestring stdout_result
      val stderr = Js.bytestring stderr_result
      val result = result
    end)

let print_deps file =
  clear_output_buffers ();
  Fun.protect ~finally:clear_output_buffers (fun () ->
    let _ = Pipeline.print_deps (Js.to_string file) in
    Js.bytestring (Buffer.contents stdout_buffer))

let add_package package dir =
  let libs = Flags.package_urls in
  libs := Flags.M.add (Js.to_string package) (Js.to_string dir) !libs

let clear_package () = Flags.package_urls := Flags.M.empty

let set_candid_path path = Flags.actor_idl_path := Some (Js.to_string path)

let set_actor_aliases entries =
  let entries = Array.map (fun kv ->
                    let kv = Js.to_array kv in
                    Js.to_string (Array.get kv 0), Js.to_string (Array.get kv 1)) (Js.to_array entries) in
  let aliases = Flags.actor_aliases in
  aliases := Flags.M.of_seq (Array.to_seq entries |> Seq.map (fun (k, p) -> k, Either.Right (p, None)))

let set_public_metadata entries =
  let entries = Array.map Js.to_string (Js.to_array entries) in
  Flags.public_metadata_names := Array.to_list entries

let gc_flags option =
  match Js.to_string option with
  | "force" -> Flags.force_gc := true
  | "scheduling" -> Flags.force_gc := false
  | "copying" -> Flags.gc_strategy := Mo_config.Flags.Copying
  | "marking" -> Flags.gc_strategy := Mo_config.Flags.MarkCompact
  | "generational" -> Flags.gc_strategy := Mo_config.Flags.Generational
  | "incremental" -> Flags.gc_strategy := Mo_config.Flags.Incremental
  | "enhancedOP" -> Flags.enhanced_orthogonal_persistence := true
  | "classicOP" -> Flags.enhanced_orthogonal_persistence := false
  | _ -> raise (Invalid_argument "gc_flags: Unexpected flag")

let js_contextual_dot_suggestions scope raw_exp =
  let open Mo_frontend in
  let open Mo_def.Syntax in
  let open Source in
  let scope = (Obj.magic scope : Scope.t) in
  let exp = (Obj.magic raw_exp : exp) in
  let receiver_ty = exp.note.note_typ in
  let libs = scope.Scope.lib_env in
  let open Typing in
  let suggestions = contextual_dot_suggestions libs receiver_ty in
  Js.array (Array.of_seq (Seq.map (fun suggestion ->
    object%js
      val moduleUri = Js.string suggestion.module_url
      val funcName = Js.string suggestion.func_name
      val funcType = Js.string (Mo_types.Type.string_of_typ suggestion.func_ty)
    end
  ) (List.to_seq suggestions)))

let js_contextual_dot_module raw_exp =
  let open Mo_frontend in
  let exp = (Obj.magic raw_exp : Mo_def.Syntax.exp) in
  match Typing.contextual_dot_module exp with
  | Some (module_name_or_uri, func_name) -> Js.Opt.return (object%js
      val moduleNameOrUri = Js.string module_name_or_uri
      val funcName = Js.string func_name
    end)
  | None -> Js.Opt.empty
