sqlcommenter serializes attributes into a trailing comment: keys and values are
url encoded, pairs are sorted, and an empty list leaves the query untouched.
Only [sql] changes, so [name] and [kind] stay usable after annotating.

  $ cat > sqlcommenter_test.ml <<'EOF'
  > let q = Sqlgg_traits.Query.make ~sql:"SELECT id FROM users WHERE id = ?" ~name:"find_user" ~kind:Sqlgg_traits.Query.(Select Nat) ()
  > let show (q : Sqlgg_traits.Query.t) = Printf.printf "name=%s sql=%s\n" q.name q.sql
  > let () =
  >   show q;
  >   show (Sqlgg_traits.Query.Sqlcommenter.annotate [ "query", q.name; "app", "billing api"; "route", "/users/1?full=1" ] q);
  >   show (Sqlgg_traits.Query.Sqlcommenter.annotate [] q);
  >   print_endline (Sqlgg_traits.Query.Sqlcommenter.comment [ "b", "2"; "a", "1" ])
  > EOF
  $ ocamlfind ocamlc -package sqlgg.traits -linkpkg -o sqlcommenter_test.exe sqlcommenter_test.ml
  $ ./sqlcommenter_test.exe
  name=find_user sql=SELECT id FROM users WHERE id = ?
  name=find_user sql=SELECT id FROM users WHERE id = ? /*app='billing%20api',query='find_user',route='%2Fusers%2F1%3Ffull%3D1'*/
  name=find_user sql=SELECT id FROM users WHERE id = ?
  /*a='1',b='2'*/

A query is a black box for the user: fields are readable, but rewriting the sql
behind the metadata is rejected, so the only way to change it is a helper.

  $ cat > rewrite.ml <<'EOF'
  > let evil (q : Sqlgg_traits.Query.t) = { q with Sqlgg_traits.Query.sql = "DROP TABLE users" }
  > EOF
  $ ocamlfind ocamlc -package sqlgg.traits -c rewrite.ml
  File "rewrite.ml", line 1, characters 38-92:
  1 | let evil (q : Sqlgg_traits.Query.t) = { q with Sqlgg_traits.Query.sql = "DROP TABLE users" }
                                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Cannot create values of the private type Sqlgg_traits.Query.t
  [2]

  $ cat > forge.ml <<'EOF'
  > let evil () = { Sqlgg_traits.Query.sql = "DROP TABLE users"; name = "find_user"; kind = Sqlgg_traits.Query.Other; filename = None }
  > EOF
  $ ocamlfind ocamlc -package sqlgg.traits -c forge.ml
  File "forge.ml", line 1, characters 14-131:
  1 | let evil () = { Sqlgg_traits.Query.sql = "DROP TABLE users"; name = "find_user"; kind = Sqlgg_traits.Query.Other; filename = None }
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Cannot create values of the private type Sqlgg_traits.Query.t
  [2]

  $ cat > append.ml <<'EOF'
  > let evil (q : Sqlgg_traits.Query.t) = Sqlgg_traits.Query.append_comment q "/*anything*/"
  > EOF
  $ ocamlfind ocamlc -package sqlgg.traits -c append.ml
  File "append.ml", line 1, characters 38-71:
  1 | let evil (q : Sqlgg_traits.Query.t) = Sqlgg_traits.Query.append_comment q "/*anything*/"
                                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unbound value Sqlgg_traits.Query.append_comment
  [2]

A value cannot escape the comment it is serialized into: only the unreserved
set survives encoding, so [*] and [/] cannot close the comment early, quotes
cannot break out of the value, and utf8 is encoded byte by byte. Duplicate keys
keep a deterministic order.

  $ cat > escaping.ml <<'EOF'
  > let () = List.iter (fun attrs -> print_endline (Sqlgg_traits.Query.Sqlcommenter.comment attrs)) [
  >   [ "route", "*/ DROP TABLE users; /*" ];
  >   [ "name", "O'Brien" ];
  >   [ "city", "\xd0\x9a\xd0\xb8\xd1\x80\xd0\xb8\xd1\x88\xd0\xb8" ];
  >   [ "k", "2"; "k", "1" ];
  >   [ "", "" ];
  > ]
  > EOF
  $ ocamlfind ocamlc -package sqlgg.traits -linkpkg -o escaping.exe escaping.ml
  $ ./escaping.exe
  /*route='%2A%2F%20DROP%20TABLE%20users%3B%20%2F%2A'*/
  /*name='O%27Brien'*/
  /*city='%D0%9A%D0%B8%D1%80%D0%B8%D1%88%D0%B8'*/
  /*k='1',k='2'*/
  /*=''*/

Annotating twice appends a second comment instead of replacing the first, so a
query that is already annotated should not be handed to another annotator.

  $ cat > twice.ml <<'EOF'
  > let q = Sqlgg_traits.Query.make ~sql:"SELECT 1" ~name:"one" ~kind:Sqlgg_traits.Query.(Select One) ()
  > let () =
  >   let q = Sqlgg_traits.Query.Sqlcommenter.annotate [ "a", "1" ] q in
  >   let q = Sqlgg_traits.Query.Sqlcommenter.annotate [ "b", "2" ] q in
  >   print_endline q.Sqlgg_traits.Query.sql
  > EOF
  $ ocamlfind ocamlc -package sqlgg.traits -linkpkg -o twice.exe twice.ml
  $ ./twice.exe
  SELECT 1 /*a='1'*/ /*b='2'*/
