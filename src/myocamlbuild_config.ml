(**
  r2 (2009-09-02)

  This ocamlbuild plugin will try to find libraries by name using (in order)
  - local myocamlbuild.config file
  - ocamlfind

  Sample myocamlbuild.config :

extlib=C:/my/contrib/extlib-1.5.1
deriving=C:/my/contrib/deriving-0.1.1/lib
oUnit=C:/my/contrib/ounit-1.0.3

*)

(** querying ocamlfind *)

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

(** querying config *)

let file_lines name =
  let l = ref [] in
  begin try
    let ch = open_in name in
    begin try while true do l := input_line ch :: !l done with End_of_file -> () end;
    close_in_noerr ch
  with
    exn -> ()
  end;
  !l

let read_config name =
  let l = file_lines name in
  let split s =
    let index = String.index s '=' in
    (String.sub s 0 index, String.sub s (index+1) ((String.length s) - index - 1))
  in
  let check path =
    if not (Sys.file_exists path) then 
      (prerr_endline ("Path not found : " ^ path); raise Not_found)
  in
  let rec loop acc = function
    | [] -> acc
    | h::t -> loop (acc @ try let x = split h in check (snd x); [x] with _ -> []) t
  in
  loop [] l

(** usage *)

let config = read_config "myocamlbuild.config"
let () =
  match config with
  | [] -> prerr_endline "No config, will use ocamlfind"
  | _ -> prerr_endline "Using config : ";
         List.iter (fun (x,y) -> Printf.eprintf "%s=%s\n%!" x y) config

let lib name =
  try
    List.assoc name config
  with exn ->
    try
      ocamlfind name
    with exn ->
      "+" ^ name

let extern ?cma pkg_name = Ocamlbuild_plugin.ocaml_lib ~extern:true ~dir:(lib pkg_name) (match cma with Some s -> s | None -> pkg_name)
