open Printf
open Sqlite3

module G = Demo_caml_gen.Sqlgg(Sqlgg_sqlite3)

let explain msg db = printf "%s : %s\n" msg (errmsg db)

let main () =
  let db = db_open ":memory:" in

  (* create tables *)
  G.create_person db;
  G.create_money db;

  (* add all person records *)
  G.add_person db "John" "Black";
  let john = last_insert_rowid db in
  G.add_person db "Ivan" "Petrov";
  let ivan = last_insert_rowid db in
  G.add_person db "Sancho" "Alvares";
  let sancho = last_insert_rowid db in

  (* add money relations *)
  G.add_money db john ivan 200L;
  G.add_money db ~src:john ~dst:sancho ~amount:100L;
  G.add_money db ~amount:250L ~dst:sancho ~src:john;
  G.add_money db sancho ivan 300L;

  (* summarize by person and output *)
  print_endline "Total transfers:";
  G.calc_total db (fun ~fullname ~total -> printf "%s = %Lu\n" fullname total);
  printf "Total total: %Lu\n" (G.Fold.calc_total db (fun ~fullname:_ ~total acc -> Int64.add acc total) 0L);

  (* list donors *)
  print_endline "Donors:";
  G.list_donors db "petrov" 100L (fun ~surname:s -> print_endline s);

  (* properly close database *)
  db_close db;

  ()

let _ = Printexc.print main ()

