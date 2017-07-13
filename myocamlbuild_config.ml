open Mybuild

let () =
  OCaml.setup();
  Version.save ~identify:false ~default:"Version_release.id" "src/version.ml";
  ()

(* mark_tag_used is not available in ocaml < 4.02 *)
(* let () = mark_tag_used "tests";; *)
