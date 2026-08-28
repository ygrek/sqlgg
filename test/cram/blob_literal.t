Values spliced into SQL as literals are serialised by the traits, and a BLOB is not
a TEXT there: `x'..'` versus `'..'`. So the literal path keeps them apart, even though
the generated host language type is `string` for both.

  $ /bin/sh ./sqlgg_test.sh blob_literal.sql blob_literal.compare.ml
  $ echo $?
  0

The two serialisations through the mock traits:

  $ cat blob_literal.sql | sqlgg -no-header -params unnamed -gen caml - > output.ml
  $ cat > check.ml <<'EOF'
  > module S = Output.Sqlgg (Print_impl)
  > 
  > let () =
  >   Print_impl.clear_mock_responses ();
  >   Print_impl.setup_select_response [];
  >   S.by_data ~datas:[ "ab" ] () (fun ~id -> ignore id);
  >   Print_impl.setup_select_response [];
  >   S.by_name ~names:[ "ab" ] () (fun ~id -> ignore id)
  > EOF
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c check.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -linkpkg -o check.exe print_impl.cmo output.cmo check.cmo
  $ ./check.exe
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT id FROM files WHERE data IN (X'6162')
  [SQL] SELECT id FROM files WHERE data IN (X'6162')
  [MOCK] Returning 0 rows
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT id FROM files WHERE name IN ('ab')
  [SQL] SELECT id FROM files WHERE name IN ('ab')
  [MOCK] Returning 0 rows
