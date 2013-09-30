open Ocamlbuild_plugin
open Command

let () =
  let bracket res destroy k = let x = (try k res with e -> destroy res; raise e) in destroy res; x in
  let get_line r d = bracket r d input_line in

  bracket (open_out "version.ml") close_out (fun out ->
   let revision =
    try
     get_line (Unix.open_process_in "git describe --always") (Unix.close_process_in)
    with
     _ -> (try get_line (open_in "version.id") close_in with _ -> "<unknown>")
   in
   Printf.fprintf out "let id=\"%s\"\n" (String.escaped revision)
  )

let () = dispatch begin function
| After_rules ->

     flag ["compile"; "ocaml"; "warn_no_44"] (S[A"-w";A"-44"]);

| _ -> ()
end
