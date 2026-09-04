open Sqlgg
open Sqlgg_lsp
open Printf

type doc = {
  path : string;
  text : string;
  lines : Line_index.t;
  document : Document.t;
}

let load path =
  let text = In_channel.with_open_bin path In_channel.input_all in
  { path; text; lines = Line_index.make text; document = Document.analyze ~path text }

let position lines offset =
  let (line, character) = Line_index.position lines offset in
  sprintf "%d:%d" (line + 1) character

let range lines (start, stop) = position lines start ^ "-" ^ position lines stop

let location doc (loc : Symbol.loc) =
  let lines = if String.equal loc.file doc.path then doc.lines else Line_index.of_file loc.file in
  loc.file ^ " " ^ range lines loc.pos

let offset doc cursor_marker =
  let (cursor_marker, skip) =
    if String.ends_with ~suffix:"^" cursor_marker then
      let cursor_marker = String.sub cursor_marker 0 (String.length cursor_marker - 1) in
      cursor_marker, String.length cursor_marker
    else cursor_marker, 0
  in
  let n = String.length cursor_marker in
  match
    Seq.init (Int.max 0 (String.length doc.text - n + 1)) Fun.id
    |> Seq.find (fun i -> String.equal (String.sub doc.text i n) cursor_marker)
  with
  | Some i -> i + skip
  | None -> failwith ("not in file: " ^ cursor_marker)

let diagnostics doc =
  Document.items doc.document
  |> List.concat_map (fun (item : Document.item) -> Document.errors item.stmt)
  |> List.iter (fun (e : Document.error) -> printf "%s %s\n" (range doc.lines e.pos) e.msg)

let tokens doc =
  Ide.semantic_tokens ~lines:doc.lines doc.document
  |> List.iter (fun (token : Ide.token) ->
    printf "%s %s\n" (range doc.lines token.pos) (Params.token_type_to_string token.typ))

let hover doc offset =
  match Ide.hover (Ide.create ~text:doc.text doc.document offset) with
  | None -> print_endline "nothing"
  | Some (value, pos) -> printf "%s\n%s" (range doc.lines pos) value

let definition doc offset =
  match Ide.definition (Ide.create ~text:doc.text doc.document offset) with
  | [] -> print_endline "nothing"
  | locs -> List.iter (fun loc -> print_endline (location doc loc)) locs

let complete doc offset =
  let (replace, items) = Completion.at ~path:doc.path doc.text offset in
  printf "replace %s\n" (range doc.lines replace);
  items
  |> List.sort (fun (a : Completion.item) b ->
    match Int.compare (Completion.Priority.order a.priority) (Completion.Priority.order b.priority) with
    | 0 -> String.compare a.label b.label
    | order -> order)
  |> List.to_seq
  |> Seq.take 12
  |> Seq.iter (fun (item : Completion.item) -> printf "%s  %s\n" item.label item.detail)

type query =
  | Diags [@as "diags"]
  | Tokens [@as "tokens"]
  | Hover [@as "hover"]
  | Definition [@as "def"]
  | Complete [@as "complete"]
[@@deriving of_string]

let parse query =
  let (name, cursor_marker) =
    match String.index_opt query ':' with
    | None -> query, None
    | Some i -> String.sub query 0 i, Some (String.sub query (i + 1) (String.length query - i - 1))
  in
  match query_of_string name with
  | Some query -> query, cursor_marker
  | None -> failwith ("unknown query " ^ query)

let run doc (query, cursor_marker) =
  let cursor = function
    | Some cursor_marker -> offset doc cursor_marker
    | None -> failwith "query needs a :MARKER"
  in
  match query with
  | Diags -> diagnostics doc
  | Tokens -> tokens doc
  | Hover -> hover doc (cursor cursor_marker)
  | Definition -> definition doc (cursor cursor_marker)
  | Complete -> complete doc (cursor cursor_marker)

let () =
  match Array.to_list Sys.argv with
  | _ :: path :: queries ->
    let doc = load path in
    List.iter (fun query -> print_endline ("### " ^ query); run doc (parse query)) queries
  | _ -> failwith "usage: ask FILE QUERY..."
