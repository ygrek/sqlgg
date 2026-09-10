open Printf

exception Failure of string

let failf fmt = ksprintf (fun msg -> raise (Failure msg)) fmt

let server_timeout = 10.
let header_separator_text = "\r\n\r\n"

type server = {
  pid : int;
  input : out_channel;
  output_fd : Unix.file_descr;
  mutable output_buffer : string;
  errors : in_channel;
  mutable reaped : bool;
}

let spawn executable =
  let (stdin_r, stdin_w) = Unix.pipe () in
  let (stdout_r, stdout_w) = Unix.pipe () in
  let (stderr_r, stderr_w) = Unix.pipe () in
  List.iter Unix.set_close_on_exec
    [ stdin_r; stdin_w; stdout_r; stdout_w; stderr_r; stderr_w ];
  let pid =
    Unix.create_process executable [| executable |] stdin_r stdout_w stderr_w
  in
  Unix.close stdin_r;
  Unix.close stdout_w;
  Unix.close stderr_w;
  {
    pid;
    input = Unix.out_channel_of_descr stdin_w;
    output_fd = stdout_r;
    output_buffer = "";
    errors = Unix.in_channel_of_descr stderr_r;
    reaped = false;
  }

let send server json =
  let body = Yojson.Safe.to_string json in
  fprintf server.input "Content-Length: %d\r\n\r\n%s%!" (String.length body) body

let read_more server =
  let buffer_size = 4096 in
  let (ready, _, _) =
    Unix.select [ server.output_fd ] [] [] server_timeout
  in
  (match ready with
   | [] -> failf "timed out waiting for an LSP message"
   | _ :: _ -> ());
  let bytes = Bytes.create buffer_size in
  match Unix.read server.output_fd bytes 0 (Bytes.length bytes) with
  | 0 -> failf "LSP server closed stdout"
  | length ->
    server.output_buffer <-
      server.output_buffer ^ Bytes.sub_string bytes 0 length

let header_separator = Re.compile (Re.str header_separator_text)

let read_message server =
  let rec header () =
    match Re.exec_opt header_separator server.output_buffer with
    | Some groups -> Re.Group.start groups 0
    | None ->
      read_more server;
      header ()
  in
  let header_length = header () in
  let headers = String.sub server.output_buffer 0 header_length in
  let content_length =
    String.split_on_char '\n' headers
    |> List.find_map (fun line ->
      match String.split_on_char ':' (String.trim line) with
      | [ name; value ] when String.equal (String.lowercase_ascii name) "content-length" ->
        Some (int_of_string (String.trim value))
      | _ -> None)
    |> Option.value ~default:(-1)
  in
  if content_length < 0 then failf "LSP response has no Content-Length header";
  let body_offset = header_length + String.length header_separator_text in
  let total_length = body_offset + content_length in
  while String.length server.output_buffer < total_length do
    read_more server
  done;
  let body = String.sub server.output_buffer body_offset content_length in
  server.output_buffer <-
    String.sub server.output_buffer total_length
      (String.length server.output_buffer - total_length);
  Yojson.Safe.from_string body

let rec receive server predicate =
  let json = read_message server in
  if predicate json then json else receive server predicate

let has_id expected = function
  | `Assoc fields ->
    begin match List.assoc_opt "id" fields with
    | Some (`Int id) -> Int.equal id expected
    | Some _ | None -> false
    end
  | _ -> false

let published satisfy = function
  | `Assoc fields ->
    begin match List.assoc_opt "method" fields, List.assoc_opt "params" fields with
    | Some (`String "textDocument/publishDiagnostics"), Some (`Assoc params) -> satisfy params
    | _ -> false
    end
  | _ -> false

let diagnostics satisfy =
  published (fun params ->
    match List.assoc_opt "diagnostics" params with
    | Some (`List diagnostics) -> satisfy diagnostics
    | Some _ | None -> false)

let versioned_diagnostics expected =
  published (fun params ->
    match List.assoc_opt "version" params, List.assoc_opt "diagnostics" params with
    | Some (`Int version), Some (`List (_ :: _)) -> Int.equal version expected
    | _ -> false)

let result = function
  | `Assoc fields -> List.assoc_opt "result" fields
  | _ -> None

let expect_null method_ response =
  match result response with
  | Some `Null -> ()
  | Some _ | None ->
    failf "%s returned a result without sqlgg.json: %s"
      method_ (Yojson.Safe.to_string response)

type 'a request = {
  jsonrpc : string;
  id : int;
  method_ : string [@key "method"];
  params : 'a;
} [@@deriving to_yojson]

type 'a notification = {
  jsonrpc : string;
  method_ : string [@key "method"];
  params : 'a;
} [@@deriving to_yojson]

type initialize_params = { capabilities : Yojson.Safe.t } [@@deriving to_yojson]

type document_id = { uri : string } [@@deriving to_yojson]

type document_item = {
  uri : string;
  language_id : string [@key "languageId"];
  version : int;
  text : string;
} [@@deriving to_yojson]

type versioned_document = { uri : string; version : int } [@@deriving to_yojson]

type content_change = { text : string } [@@deriving to_yojson]

type position = { line : int; character : int } [@@deriving to_yojson]

type document_params = {
  text_document : document_id [@key "textDocument"];
} [@@deriving to_yojson]

type document_position_params = {
  text_document : document_id [@key "textDocument"];
  position : position;
} [@@deriving to_yojson]

type did_open_params = {
  text_document : document_item [@key "textDocument"];
} [@@deriving to_yojson]

type did_change_params = {
  text_document : versioned_document [@key "textDocument"];
  content_changes : content_change list [@key "contentChanges"];
} [@@deriving to_yojson]

let no_params () = `Null

let request server ~id ~method_ params_to_yojson params =
  send server
    (request_to_yojson params_to_yojson { jsonrpc = "2.0"; id; method_; params });
  receive server (has_id id)

let notify server ~method_ params_to_yojson params =
  send server
    (notification_to_yojson params_to_yojson { jsonrpc = "2.0"; method_; params })

let initialize server =
  ignore
    (request server ~id:1 ~method_:"initialize" initialize_params_to_yojson
      { capabilities = `Assoc [] })

let shutdown server id =
  ignore (request server ~id ~method_:"shutdown" no_params ());
  notify server ~method_:"exit" no_params ()

let wait_for_exit server =
  let poll_interval = 0.01 in
  let deadline = Unix.gettimeofday () +. server_timeout in
  let rec loop () =
    match Unix.waitpid [ Unix.WNOHANG ] server.pid with
    | 0, _ when Unix.gettimeofday () < deadline ->
      Unix.sleepf poll_interval;
      loop ()
    | 0, _ -> failf "server did not exit after the exit notification"
    | _, Unix.WEXITED 0 -> server.reaped <- true
    | _, Unix.WEXITED code ->
      server.reaped <- true;
      failf "server exited with code %d" code
    | _, Unix.WSIGNALED signal | _, Unix.WSTOPPED signal ->
      server.reaped <- true;
      failf "server stopped by signal %d" signal
  in
  loop ()

let close server =
  close_out_noerr server.input;
  if not server.reaped then begin
    (try Unix.kill server.pid Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
    ignore (Unix.waitpid [] server.pid);
    server.reaped <- true
  end;
  Unix.close server.output_fd;
  close_in_noerr server.errors

let with_server executable f =
  let server = spawn executable in
  match f server with
  | () -> close server
  | exception exn ->
    close server;
    raise exn

let did_open server ~uri ~text =
  notify server ~method_:"textDocument/didOpen" did_open_params_to_yojson
    { text_document = { uri; language_id = "sql"; version = 1; text } }

let did_close server ~uri =
  notify server ~method_:"textDocument/didClose" document_params_to_yojson
    { text_document = { uri } }

let project_uri name =
  "file://" ^ Filename.concat (Unix.getcwd ()) name

let test_shutdown executable =
  with_server executable (fun server ->
    initialize server;
    shutdown server 2;
    wait_for_exit server)

let test_close_clears_diagnostics executable =
  with_server executable (fun server ->
    let uri = project_uri "invalid.sql" in
    initialize server;
    did_open server ~uri ~text:"SELECT FROM;";
    ignore (receive server (diagnostics (function [] -> false | _ :: _ -> true)));
    did_close server ~uri;
    ignore (receive server (diagnostics (function [] -> true | _ :: _ -> false)));
    shutdown server 2;
    close_out_noerr server.input)

let test_close_forgets_document executable =
  with_server executable (fun server ->
    let uri = project_uri "closed.sql" in
    initialize server;
    did_open server ~uri ~text:"SELECT 1;";
    ignore (receive server (diagnostics (fun _ -> true)));
    did_close server ~uri;
    let response =
      request server ~id:2 ~method_:"textDocument/semanticTokens/full"
        document_params_to_yojson { text_document = { uri } }
    in
    (match result response with
     | Some `Null -> ()
     | Some _ | None ->
       failf "closed document remains requestable: %s" (Yojson.Safe.to_string response));
    shutdown server 3;
    wait_for_exit server)

let test_change_versions_diagnostics executable =
  with_server executable (fun server ->
    let uri = project_uri "version.sql" in
    initialize server;
    did_open server ~uri ~text:"SELECT FROM;";
    ignore (receive server (versioned_diagnostics 1));
    notify server ~method_:"textDocument/didChange" did_change_params_to_yojson
      { text_document = { uri; version = 2 };
        content_changes = [ { text = "SELECT WHERE;" } ] };
    ignore (receive server (versioned_diagnostics 2));
    shutdown server 2;
    close_out_noerr server.input)

let test_hover executable =
  with_server executable (fun server ->
    let uri = project_uri "hover.sql" in
    initialize server;
    did_open server ~uri ~text:"SELECT 1;";
    ignore (receive server (diagnostics (fun _ -> true)));
    ignore
      (request server ~id:2 ~method_:"textDocument/hover"
        document_position_params_to_yojson
        { text_document = { uri }; position = { line = 0; character = 7 } });
    shutdown server 3;
    close_out_noerr server.input)

let test_no_project executable =
  with_server executable (fun server ->
    let uri = "file:///tmp/sqlgg-lsp-no-project.sql" in
    initialize server;
    did_open server ~uri ~text:"SELECT missing FROM absent;";
    ignore (receive server (diagnostics (function [] -> true | _ :: _ -> false)));
    let position method_ id =
      request server ~id ~method_
        document_position_params_to_yojson
        { text_document = { uri }; position = { line = 0; character = 8 } }
    in
    expect_null "hover" (position "textDocument/hover" 2);
    expect_null "definition" (position "textDocument/definition" 3);
    expect_null "completion" (position "textDocument/completion" 4);
    expect_null "semanticTokens"
      (request server ~id:5 ~method_:"textDocument/semanticTokens/full"
        document_params_to_yojson { text_document = { uri } });
    shutdown server 6;
    close_out_noerr server.input)

let () =
  match Array.to_list Sys.argv with
  | [ _; executable; "shutdown" ] -> test_shutdown executable
  | [ _; executable; "close-clears-diagnostics" ] ->
    test_close_clears_diagnostics executable
  | [ _; executable; "close-forgets-document" ] ->
    test_close_forgets_document executable
  | [ _; executable; "change-versions-diagnostics" ] ->
    test_change_versions_diagnostics executable
  | [ _; executable; "hover" ] -> test_hover executable
  | [ _; executable; "no-project" ] -> test_no_project executable
  | _ ->
    failf
      "usage: protocol LSP \
       {shutdown|close-clears-diagnostics|close-forgets-document|change-versions-diagnostics|hover|no-project}"
