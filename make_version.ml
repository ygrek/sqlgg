(* $Id$ *)

let _ = 
  let str = input_line stdin in
  print_endline ("let revision=\"" ^ str ^ "\"");
  print_endline "let version=\"0.2.0\""
