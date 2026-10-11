
type mode = Normal | Ignore | Ident
let mode = ref Normal
let mode_normal () = mode := Normal
let mode_ignore () = mode := Ignore
let mode_ident () = mode := Ident
module Stmt_metadata = struct
  let stmt_metadata: (int, (string * string) list) Hashtbl.t = Hashtbl.create 16

  let add k v = Hashtbl.add stmt_metadata k v
  let find_all k = Hashtbl.find_all stmt_metadata k
  let reset () = Hashtbl.reset stmt_metadata
end

(* Menhir semantic actions cannot take arguments, so the statement being parsed is scoped
   to the parse call. *)
type parse = { lexbuf : Lexing.lexbuf; semantics : Dialect.Semantic.t }

let current : parse option ref = ref None

let with_parse parse f =
  let saved = !current in
  current := Some parse;
  Fun.protect f ~finally:(fun () -> current := saved)

let semantics () =
  match !current with
  | Some { semantics; _ } -> semantics
  | None -> failwith "SQL grammar actions ran outside Parser_state.with_parse"

let extract_source (start_, end_) =
  Stdlib.Option.bind !current (fun { lexbuf; _ } ->
    let len = end_ - start_ in
    if len > 0 && start_ >= 0 && end_ <= lexbuf.Lexing.lex_buffer_len then
      Some (Bytes.sub_string lexbuf.lex_buffer start_ len)
    else
      None)
