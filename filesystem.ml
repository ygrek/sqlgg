(* $Id$ *)

let is_dir path = try Some (Sys.is_directory path) with exn -> None
let read_dir path = try Sys.readdir path with exn -> [||]

let rec of_dir path l = 
  let names = read_dir path in 
  let fullname name = Filename.concat path name in
  Array.fold_left (fun ll name -> add_entry (fullname name) ll) l names
and
 add_entry path (l1,l2) =
   match is_dir path with
   | Some true -> of_dir path (l1,l2)
   | Some false -> let z1,z2 = xsd_of_file path in (z1@l1),(z2@l2)
   | None -> l1,l2

let norm_path str = 
  let b = (String.starts_with str "/") || (String.starts_with str "\\") in
  let s = String.strip ~chars:"/\\" str in
  if b then "\\" ^ s else s
