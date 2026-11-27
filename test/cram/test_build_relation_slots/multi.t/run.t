Two independently chosen relations joined by l.id = r.id share the SQL kind of
the joined columns as an index, so an INT relation cannot meet a TEXT one. No
pair of candidates is enumerated for that.

  $ sqlgg -no-header -gen caml -params unnamed -dialect mysql multi.sql > multi.ml
  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -i multi.ml > multi_api.txt
  $ sed -n '/module Pair/,/module Single/p' multi_api.txt
        module Pair :
          sig
            module Left_src :
              sig
                type _ t = private
                    Table_users : [ `Int ] t
                  | Table_teams : [ `Int ] t
                  | Table_tags : [ `Text ] t
                val table_users : [ `Int ] t
                val table_teams : [ `Int ] t
                val table_tags : [ `Text ] t
              end
            module Right_src :
              sig
                type _ t = private
                    Table_users : [ `Int ] t
                  | Table_teams : [ `Int ] t
                  | Table_tags : [ `Text ] t
                val table_users : [ `Int ] t
                val table_teams : [ `Int ] t
                val table_tags : [ `Text ] t
              end
            val select :
              [> `RO ] T.connection ->
              left_src:'x Left_src.t ->
              right_src:'x Right_src.t -> T.Types.Int.t
            module Single :

The same slot used twice is one choice; its column type is an index:

  $ sed -n '/module Tree/,/val select/p' multi_api.txt
        module Tree :
          sig
            module Src :
              sig
                type _ t = private
                    Table_users : T.Types.Int.t t
                  | Table_teams : T.Types.Int.t t
                  | Table_tags : T.Types.Text.t t
                val table_users : T.Types.Int.t t
                val table_teams : T.Types.Int.t t
                val table_tags : T.Types.Text.t t
              end
            val select :

A slot inside a correlated subquery only accepts relations with the referenced column:

  $ sed -n '/module Active/,/val select/p' multi_api.txt
        module Active :
          sig
            module Ev :
              sig type t = private Table_events : t val table_events : t end
            val select :
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c multi.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe multi.cmo print_impl.cmo run.ml
  $ ./run.exe | grep -E '^===|PREPARE'
  === pair users teams ===
  [MOCK] PREPARE[1]: SELECT COUNT(1) AS n FROM `users` AS l JOIN `teams` AS r ON l.id = r.id
  === tree over tags ===
  [MOCK] PREPARE[2]: SELECT child.id FROM `tags` AS child JOIN `tags` AS parent ON child.parent_id = parent.id
  === correlated EXISTS ===
  [MOCK] PREPARE[3]: SELECT u.id FROM users AS u WHERE EXISTS (SELECT 1 FROM `events` AS e WHERE e.user_id = u.id)

  $ ocamlfind ocamlc -package sqlgg.traits -I . -c incompatible.ml 2>&1 | grep -E '^Error|no intersection'
  Error: This expression has type
         These two variant types have no intersection
