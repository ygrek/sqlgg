open Mybuild

let () =
  OCaml.setup();
  Version.save ~default:"Version_release.id" "version.ml";
  ()

(* mark_tag_used is not available in ocaml < 4.02 *)
(* let () = mark_tag_used "tests";; *)
