(* modify as needed *)

let chomp s =
  let is_nl ch = match ch with | '\n' | '\r' -> true | _ -> false in
  let rec cut n =
    if n = 0 then 0 else if is_nl s.[n-1] then cut (n-1) else n
  in
  let ls = String.length s in
  let n = cut ls in
  if n = ls then s else String.sub s 0 n

let ocamlfind lib =
  let cin = Unix.open_process_in (Printf.sprintf "ocamlfind -query %s" lib) in
  let s = chomp (input_line cin) in
(*  let s = Filename.quote s in*)
  ignore (Unix.close_process_in cin);
  s

let extlib_dir = ocamlfind "extlib"
let ounit_dir = ocamlfind "oUnit"

let deriving_dir = 
  if Sys.os_type = "Win32"
  then "D:/temp/deriving-ocaml-3.11/deriving-0.1.1/lib"
  else "/home/ygrek/work/contrib/deriving-0.1.1/lib"
