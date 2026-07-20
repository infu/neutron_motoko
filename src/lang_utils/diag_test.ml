open Source
open Diag

let message text = Diag.info_message no_region "test" text

let texts messages = List.map (fun (message : Diag.message) -> message.text) messages

let%test_unit "traverse is stack-safe and preserves value/message order" =
  let inputs = Lib.List.table 100_000 Fun.id in
  match Diag.traverse (fun value -> Ok (value, [message (string_of_int value)])) inputs with
  | Error _ -> assert false
  | Ok (values, messages) ->
    assert (List.length values = 100_000);
    assert (List.hd values = 0);
    assert (List.hd (List.rev values) = 99_999);
    assert ((List.hd messages).text = "0");
    assert ((List.hd (List.rev messages)).text = "99999")

let%test_unit "traverse stops at the first error and keeps prior messages" =
  let calls = ref [] in
  let visit value =
    calls := value :: !calls;
    if value = 2 then Error [message "error"]
    else Ok (value, [message ("ok" ^ string_of_int value)])
  in
  match Diag.traverse visit [0; 1; 2; 3] with
  | Ok _ -> assert false
  | Error messages ->
    assert (List.rev !calls = [0; 1; 2]);
    assert (texts messages = ["ok0"; "ok1"; "error"])

let%test_unit "traverse_ is stack-safe and preserves messages" =
  let inputs = Lib.List.table 100_000 Fun.id in
  let calls = ref 0 in
  match Diag.traverse_
    (fun value ->
      incr calls;
      Ok ((), if value mod 25_000 = 0 then [message (string_of_int value)] else []))
    inputs with
  | Error _ -> assert false
  | Ok ((), messages) ->
    assert (!calls = 100_000);
    assert (texts messages = ["0"; "25000"; "50000"; "75000"])

let%test_unit "fold is stack-safe and preserves messages" =
  let inputs = Lib.List.table 100_000 Fun.id in
  match Diag.fold
    (fun count value -> Ok (count + 1, if value mod 25_000 = 0 then [message (string_of_int value)] else []))
    0 inputs with
  | Error _ -> assert false
  | Ok (count, messages) ->
    assert (count = 100_000);
    assert (texts messages = ["0"; "25000"; "50000"; "75000"])

let%test_unit "direct warnings honor disabled warning levels" =
  let previous = Mo_config.Flags.get_warning_level "M0005" in
  Fun.protect
    ~finally:(fun () -> Mo_config.Flags.set_warning_level "M0005" previous)
    (fun () ->
      Mo_config.Flags.set_warning_level "M0005" Mo_config.Flags.Allow;
      match Diag.warn no_region "M0005" "import" "case mismatch" with
      | Ok ((), []) -> ()
      | _ -> assert false)
