
open Printf
open Sqlite3

(* module G = Sqlgg(Sqlgg_sqlite3) *)

let explain msg db = printf "%s : %s\n" msg (errmsg db)

let main () =
  let db = db_open "test.db" in
  explain "open" db;

  exec db "DROP TABLE test;";
  explain "drop" db;
  exec db "DROP TABLE loc;";
  explain "drop" db;
  exec db "DROP TABLE zuzu;";
  explain "drop" db;

(*
  G.create db;
  explain "create" db;

  G.add db "c++" "ugly";
  explain "insert" db;

  G.add db "c" "hard";
  explain "insert" db;

  G.add db "ocaml" "wonderful";
  explain "insert" db;

  G.exaggerate db "really";
  explain "update" db;

  G.select_all db (fun id name descr -> printf "%u) %s is %s\n" id name descr);
  explain "select" db;

  G.create_loc db;
  explain "create_loc" db;

  G.create_zuzu db "qq";
  explain "create_zuzu" db;
*)

  let ok = db_close db in
  printf "close: %B\n" ok;

  ()

let _ = 
  Printexc.print main ()

