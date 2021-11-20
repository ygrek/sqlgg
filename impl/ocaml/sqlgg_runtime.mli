(**
  OCaml runtime support for sqlgg + generated code
  2021-11-10

  This is free and unencumbered software released into the public domain.

  Anyone is free to copy, modify, publish, use, compile, sell, or
  distribute this software, either in source code form or as a compiled
  binary, for any purpose, commercial or non-commercial, and by any
  means.

  For more information, please refer to <http://unlicense.org/>
*)

(* Returns a MySQL string literal for the supplied string. *)
val mysql_string_to_literal : string -> string

(* Returns a Sqlite3 string literal for the supplied string. *)
val sqlite3_string_to_literal : string -> string
