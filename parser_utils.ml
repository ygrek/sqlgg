(* $Id$ *)

module type Parser_type =
sig
  type token
  type result
  val input : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> result
  val rule : Lexing.lexbuf -> token
end

module Make(T : Parser_type) =
struct
  let parse_buf_exn lexbuf =
    try
      T.input T.rule lexbuf
    with exn -> 
      begin
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        let extra = Printexc.to_string exn in
        Error.report "Exception %s in %u:%u at lexeme \"%s\"" extra line cnum tok;
        raise exn
      end

  let parse_buf lexbuf = try Some (parse_buf_exn lexbuf) with exn -> None

  let parse_stdin () = parse_buf (Lexing.from_channel stdin)
  let parse_string str = (*Error.log "Parsing : %s" str; *)
                         parse_buf (Lexing.from_string str)

  let parse_file filename =
    let contents = try Std.input_file filename with exn -> "" in
    parse_string contents

end
