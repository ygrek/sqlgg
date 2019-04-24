
open Printf
module M = Mysql

module G = Demo_caml_gen_mysql.Sqlgg(Sqlgg_mysql.Make(Int64))

let main () =
  let db = M.quick_connect ~database:"test" ~user:"root" () in

  G.drop_person db;
  G.drop_money db;

  (* create tables *)
  G.create_person db;
  G.create_money db;

  (* add all person records *)
  G.add_person db "John" "Black";
  let john = M.insert_id db in
  G.add_person db "Ivan" "Petrov";
  let ivan = M.insert_id db in
  G.add_person db "Sancho" "Alvares";
  let sancho = M.insert_id db in

  (* add money relations *)
  G.add_money db john ivan 200L;
  G.add_money db ~src:john ~dst:sancho ~amount:100L;
  G.add_money db ~amount:250L ~dst:sancho ~src:john;
  G.add_money db sancho ivan 300L;

  (* summarize by person and output *)
  print_endline "Total transfers:";
  G.calc_total db (fun ~fullname ~total -> printf "%s = %Lu\n" fullname total);

  (* list donors *)
  print_endline "Donors:";
  G.list_donors db "petrov" 100L (fun ~surname:s -> print_endline s);

  (* properly close database *)
  M.disconnect db;

  ()

let () = Printexc.print main ()

