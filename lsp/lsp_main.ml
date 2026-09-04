open Sqlgg
open Sqlgg_lsp
open Linol_lwt

let position lines offset =
  let (line, character) = Line_index.position lines offset in
  Position.create ~line ~character

let range lines (start, stop) =
  Range.create ~start:(position lines start) ~end_:(position lines stop)

class sqlgg_lsp =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server as super

    method spawn_query_handler f = Linol_lwt.spawn f

    method! config_hover = Some (`Bool true)
    method! config_definition = Some (`Bool true)

    method! config_completion =
      Some (CompletionOptions.create ~triggerCharacters:[ "."; "@" ] ())

    method! config_modify_capabilities (capabilities : ServerCapabilities.t) =
      let types = List.map Params.token_type_to_string Params.all_of_token_type in
      let legend = SemanticTokensLegend.create ~tokenTypes:types ~tokenModifiers:[] in
      { capabilities with
        semanticTokensProvider =
          Some (`SemanticTokensOptions (SemanticTokensOptions.create ~legend ~full:(`Bool true) ())) }

    val cache = Document.Cache.create ()

    method private document uri content = Document.analyze ~cache ~path:(DocumentUri.to_path uri) content

    val mutable last_lines : (string * Line_index.t) option = None

    method private lines content =
      match last_lines with
      | Some (text, lines) when String.equal text content -> lines
      | Some _ | None ->
        let lines = Line_index.make ~encoding:positionEncoding content in
        last_lines <- Some (content, lines);
        lines

    method private offset content (pos : Position.t) =
      let lines = self#lines content in
      lines, Line_index.offset lines ~line:pos.line ~character:pos.character

    method private cursor uri content pos =
      let (lines, offset) = self#offset content pos in
      lines, Ide.create ~text:content (self#document uri content) offset

    method private location ~here ~lines (loc : Symbol.loc) =
      let file = loc.file in
      let lines =
        if String.equal file here then Some lines
        else
          match Line_index.of_file ~encoding:positionEncoding file with
          | lines -> Some lines
          | exception Sys_error _ -> None
      in
      lines |> Option.map (fun lines ->
        Location.create ~uri:(DocumentUri.of_path file) ~range:(range lines loc.pos))

    method private publish ~(notify_back : Linol_lwt.Jsonrpc2.notify_back) uri content =
      let lines = self#lines content in
      Document.items (self#document uri content)
      |> List.concat_map (fun (item : Document.item) -> Document.errors item.stmt)
      |> List.map (fun (e : Document.error) ->
        Diagnostic.create ~range:(range lines e.pos) ~severity:DiagnosticSeverity.Error
          ~source:"sqlgg" ~message:(`String e.msg) ())
      |> notify_back#send_diagnostic

    method on_notif_doc_did_open ~notify_back doc ~content =
      self#publish ~notify_back doc.TextDocumentItem.uri content

    method on_notif_doc_did_change ~notify_back doc _changes ~old_content:_ ~new_content =
      self#publish ~notify_back doc.VersionedTextDocumentIdentifier.uri new_content

    method on_notif_doc_did_close ~notify_back:_ doc =
      Document.Cache.forget cache (DocumentUri.to_path doc.TextDocumentIdentifier.uri);
      Linol_lwt.return ()

    method! on_request_unhandled : type r.
        notify_back:Linol_lwt.Jsonrpc2.notify_back ->
        id:Linol_lwt.Jsonrpc2.Req_id.t ->
        r Linol.Lsp.Client_request.t ->
        r Linol_lwt.t =
      fun ~notify_back ~id req ->
        match req with
        | Linol.Lsp.Client_request.SemanticTokensFull { textDocument = doc; _ } ->
          let uri = doc.TextDocumentIdentifier.uri in
          begin match self#find_doc uri with
          | None -> Linol_lwt.return None
          | Some doc ->
            let content = doc.Linol_lwt.Jsonrpc2.content in
            let lines = self#lines content in
            let delta (prev : Position.t) (token : Ide.token) =
              let pos = position lines (fst token.pos) and stop = position lines (snd token.pos) in
              let delta_line = pos.line - prev.line in
              let delta_char = if delta_line = 0 then pos.character - prev.character else pos.character in
              pos, [ delta_line; delta_char; stop.character - pos.character; Params.token_type_to_enum token.typ; 0 ]
            in
            let data =
              Ide.semantic_tokens ~lines (self#document uri content)
              |> List.fold_left_map delta (Position.create ~line:0 ~character:0)
              |> snd
              |> List.concat
            in
            Linol_lwt.return (Some (SemanticTokens.create ~data:(Array.of_list data) ()))
          end
        | _ -> super#on_request_unhandled ~notify_back ~id req

    method! on_req_hover ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_ doc =
      let (lines, cursor) = self#cursor uri doc.Linol_lwt.Jsonrpc2.content pos in
      Ide.hover cursor
      |> Option.map (fun (value, pos) ->
        let contents = `MarkupContent (MarkupContent.create ~kind:MarkupKind.Markdown ~value) in
        Hover.create ~contents ~range:(range lines pos) ())
      |> Linol_lwt.return

    method! on_req_definition ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_
        ~partialResultToken:_ doc =
      let (lines, cursor) = self#cursor uri doc.Linol_lwt.Jsonrpc2.content pos in
      let here = DocumentUri.to_path uri in
      match List.filter_map (self#location ~here ~lines) (Ide.definition cursor) with
      | [] -> Linol_lwt.return None
      | locs -> Linol_lwt.return (Some (`Location locs))

    method! on_req_completion ~notify_back:_ ~id:_ ~uri ~pos ~ctx:_
        ~workDoneToken:_ ~partialResultToken:_ doc =
      let content = doc.Linol_lwt.Jsonrpc2.content in
      let (lines, offset) = self#offset content pos in
      let (replace, items) = Completion.at ~cache ~path:(DocumentUri.to_path uri) content offset in
      match items with
      | [] -> Linol_lwt.return None
      | items ->
        let range = range lines replace in
        let item (item : Completion.item) =
          CompletionItem.create ~label:item.label ~detail:item.detail ~kind:item.kind
            ~sortText:(Printf.sprintf "%02d%s" (Completion.Priority.order item.priority) item.label)
            ~textEdit:(`TextEdit (TextEdit.create ~range ~newText:item.label))
            ()
        in
        Linol_lwt.return (Some (`List (List.map item items)))
  end

let () =
  let server = new sqlgg_lsp in
  let rpc = Linol_lwt.Jsonrpc2.create_stdio ~env:() server in
  match Linol_lwt.run (Linol_lwt.Jsonrpc2.run ~shutdown:(fun () -> server#must_quit) rpc) with
  | () -> ()
  | exception exn ->
    Printf.eprintf "sqlgg-lsp fatal: %s\n%!" (Printexc.to_string exn);
    exit 1
