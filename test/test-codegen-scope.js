// Run with: nix develop -c node test/test-codegen-scope.js
// These implementation details are intentionally not exposed by codegen's .mli.
// Extract the actual functions, then execute them with small OCaml stand-ins for
// their dependencies. Missing/renamed source boundaries fail rather than silently
// testing a stale copy of the implementation.
const assert = require("node:assert/strict");
const { readFileSync } = require("node:fs");
const { join } = require("node:path");
const { spawnSync } = require("node:child_process");

const implementations = ["classical", "enhanced"].map((backend) => {
  const source = readFileSync(
    join(__dirname, "../src/codegen", `compile_${backend}.ml`), "utf8",
  );
  const capture = [...source.matchAll(
    /^  let how_captured\b[\s\S]*?(?=^  let stackrep_of_type\b)/gm,
  )];
  const declarations = [...source.matchAll(
    /^and compile_decs_public\b[\s\S]*?(?=^and compile_decs\b)/gm,
  )];
  assert.equal(capture.length, 1, `${backend}: how_captured source boundary`);
  assert.equal(declarations.length, 1, `${backend}: compile_decs_public source boundary`);
  return `module ${backend === "classical" ? "Classical" : "Enhanced"} = struct
${capture[0][0]}
${declarations[0][0].replace(/^and /, "let ")}
end`;
});

const program = `
module M = Map.Make(String)
module S = Set.Make(String)
module VarEnv = struct
  type t = int
  type level = TopLvl | NotTopLvl
end
type how = Const | LocalImmut of int | LocalMut of int | StoreHeap | StoreStatic

type code = Empty | Token of int | Sequence of code * code
module G = struct let nop = Empty end
let (^^) left right = Sequence (left, right)
type scope_wrap = code -> code
let unmodified body = body
type phase = Analyze | Compile | Make | Apply
exception Injected of phase * int
let events = ref []
let failure = ref None
let event phase declaration environment =
  events := (phase, declaration, environment) :: !events;
  if !failure = Some (phase, declaration) then raise (Injected (phase, declaration))

module AllocHow = struct
  let decs pre_ae declarations captured =
    assert (captured = 43);
    event Analyze (List.length declarations) pre_ae
end

let compile_dec env pre_ae how v2en declaration =
  assert (env = 41 && how = () && v2en = 42);
  event Compile declaration pre_ae;
  (pre_ae + declaration + 1, Token (3 * declaration), fun final_ae ->
    event Make declaration final_ae;
    fun body ->
      event Apply declaration final_ae;
      Token (3 * declaration + 1) ^^ body ^^ Token (3 * declaration + 2))

${implementations.join("\n")}

let check label condition =
  if not condition then failwith ("codegen scope test: " ^ label)

let captures label how_captured =
  let states = [| Const; LocalImmut 0; LocalImmut 1; LocalMut 0;
                  LocalMut 1; StoreHeap; StoreStatic |] in
  let check_case how seen captured =
    let defined = M.fold (fun name _ result -> S.add name result) how S.empty in
    List.iter (fun level ->
      let direct = how_captured level how seen captured in
      let filtered = how_captured level how seen (S.inter defined captured) in
      check (label ^ ": unfiltered captured set") (M.equal (=) direct filtered)
    ) [VarEnv.TopLvl; VarEnv.NotTopLvl]
  in
  check_case M.empty S.empty S.empty;
  check_case M.empty S.empty (S.of_list ["missing"; "also_missing"]);
  Array.iter (fun state ->
    List.iter (fun was_seen ->
      List.iter (fun was_captured ->
        let how = M.singleton "present" state in
        let seen = if was_seen then S.singleton "present" else S.empty in
        let captured = S.of_list
          (if was_captured then ["present"; "absent"] else ["absent"]) in
        check_case how seen captured;
        let expected_local = match state with
          | LocalMut _ when was_captured -> M.singleton "present" StoreHeap
          | LocalImmut _ when was_captured && not was_seen ->
            M.singleton "present" StoreHeap
          | _ -> M.empty in
        let expected_top = match state with
          | LocalMut _ | LocalImmut _ when was_captured ->
            M.singleton "present" StoreStatic
          | _ -> M.empty in
        check (label ^ ": local promotion")
          (M.equal (=) expected_local
            (how_captured VarEnv.NotTopLvl how seen captured));
        check (label ^ ": top-level promotion")
          (M.equal (=) expected_top
            (how_captured VarEnv.TopLvl how seen captured))
      ) [false; true]
    ) [false; true]
  ) states;
  let random = Random.State.make [| 0xCA97; 0x2026 |] in
  for _ = 1 to 1_000 do
    let how = ref M.empty and seen = ref S.empty and captured = ref S.empty in
    for index = 0 to 127 do
      let name = string_of_int index in
      if Random.State.bool random then
        how := M.add name states.(Random.State.int random (Array.length states)) !how;
      if Random.State.bool random then seen := S.add name !seen;
      if Random.State.bool random then captured := S.add name !captured
    done;
    check_case !how !seen !captured
  done

(* The pre-refactor recursive composition, retained only as an order oracle. *)
let recursive_compile_decs_public env pre_ae decs v2en captured_in_body =
  let how = AllocHow.decs pre_ae decs captured_in_body in
  let rec go pre_ae = function
    | [] -> (pre_ae, G.nop, fun _ -> unmodified)
    | [dec] -> compile_dec env pre_ae how v2en dec
    | dec :: decs ->
      let (pre_ae1, alloc_code1, mk_codeW1) = compile_dec env pre_ae how v2en dec in
      let (pre_ae2, alloc_code2, mk_codeW2) = go pre_ae1 decs in
      (pre_ae2, alloc_code1 ^^ alloc_code2, fun ae ->
        let codeW1 = mk_codeW1 ae in
        let codeW2 = mk_codeW2 ae in
        fun body_code -> codeW1 (codeW2 body_code))
  in
  let (ae1, alloc_code, mk_codeW) = go pre_ae decs in
  (ae1, fun body_code -> alloc_code ^^ mk_codeW ae1 body_code)

let flatten root =
  let rec go pending reversed = match pending with
    | [] -> List.rev reversed
    | Empty :: rest -> go rest reversed
    | Token value :: rest -> go rest (value :: reversed)
    | Sequence (left, right) :: rest -> go (left :: right :: rest) reversed
  in go [root] []

let run compile failure_at declarations =
  events := [];
  failure := failure_at;
  let outcome =
    try
      let final_ae, wrap = compile 41 7 declarations 42 43 in
      (* Applying twice also verifies that composing a scope doesn't consume it. *)
      let first = flatten (wrap (Token (-1))) in
      let second = flatten (wrap (Token (-2))) in
      Ok (final_ae, first, second)
    with Injected (phase, declaration) -> Error (phase, declaration)
  in
  outcome, List.rev !events

let declarations label compile =
  let equivalent failure declarations =
    let expected = run recursive_compile_decs_public failure declarations in
    let actual = run compile failure declarations in
    check (label ^ ": declaration output/events/exception order") (actual = expected)
  in
  List.iter (equivalent None) [ []; [0]; [0; 1]; [0; 1; 2; 3; 4]; [2; 0; 2] ];
  let random = Random.State.make [| 0xDEC5 |] in
  for _ = 1 to 100 do
    equivalent None (List.init (Random.State.int random 200)
      (fun _ -> Random.State.int random 100))
  done;
  let decs = [0; 1; 2; 3; 4] in
  equivalent (Some (Analyze, List.length decs)) decs;
  List.iter (fun phase ->
    List.iter (fun declaration -> equivalent (Some (phase, declaration)) decs) decs
  ) [Compile; Make; Apply];

  (* Do not run the recursive oracle at this size: that is the bug being fixed. *)
  let count = 100_000 in
  let decs = List.init count Fun.id in
  let outcome, trace = run compile None decs in
  let final_ae = 7 + count * (count + 1) / 2 in
  let verify_output body output =
    let remaining = ref output in
    let take expected = match !remaining with
      | actual :: rest -> check (label ^ ": large output") (actual = expected); remaining := rest
      | [] -> failwith (label ^ ": truncated output")
    in
    for index = 0 to count - 1 do take (3 * index) done;
    for index = 0 to count - 1 do take (3 * index + 1) done;
    take body;
    for index = count - 1 downto 0 do take (3 * index + 2) done;
    check (label ^ ": output exhausted") (!remaining = [])
  in
  (match outcome with
   | Ok (actual_ae, first, second) ->
     check (label ^ ": final environment") (actual_ae = final_ae);
     verify_output (-1) first;
     verify_output (-2) second
   | Error _ -> failwith (label ^ ": unexpected injected failure"));
  let remaining = ref trace in
  let take expected = match !remaining with
    | actual :: rest -> check (label ^ ": large event order") (actual = expected); remaining := rest
    | [] -> failwith (label ^ ": truncated events")
  in
  take (Analyze, count, 7);
  for index = 0 to count - 1 do take (Compile, index, 7 + index * (index + 1) / 2) done;
  for _ = 1 to 2 do
    for index = 0 to count - 1 do take (Make, index, final_ae) done;
    for index = count - 1 downto 0 do take (Apply, index, final_ae) done
  done;
  check (label ^ ": events exhausted") (!remaining = [])

let () =
  captures "classical" Classical.how_captured;
  captures "enhanced" Enhanced.how_captured;
  declarations "classical" Classical.compile_decs_public;
  declarations "enhanced" Enhanced.compile_decs_public;
  print_endline "Both code generators: captured-set equivalence and 100,000 declaration scope tests passed"
`;

const result = spawnSync(process.env.OCAML || "ocaml", ["-noinit", "-stdin"], {
  input: program,
  encoding: "utf8",
  // Keep the test stack bounded so native OCaml's growable stack cannot hide a
  // recursion regression that would overflow the packaged browser compiler.
  env: {
    ...process.env,
    OCAMLRUNPARAM: [process.env.OCAMLRUNPARAM, "l=128k"].filter(Boolean).join(","),
  },
});
if (result.error) throw result.error;
process.stdout.write(result.stdout || "");
process.stderr.write(result.stderr || "");
assert.equal(result.signal, null, `OCaml terminated by ${result.signal}`);
assert.equal(result.status, 0, "OCaml codegen scope regression failed");
