type  t = (string, string * Sql.select_full) Hashtbl.t
let shared_queries: t = Hashtbl.create 15

let add = Hashtbl.add shared_queries

let get name = try Hashtbl.find shared_queries name with Not_found -> 
  failwith @@ Printf.sprintf "Shared query %s not found" name

let mem name = Hashtbl.mem shared_queries name
