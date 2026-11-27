Reusable SELECTs are candidates too. Their own parameters belong to the builder,
not to the outer function. Candidates using subst or choices are not offered.

  $ sqlgg -no-header -gen caml -params unnamed -dialect mysql reusable.sql > reusable.ml
  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -i reusable.ml | sed -n '/module Pick/,/module Fold/p'
        module Pick :
          sig
            module Src :
              sig
                type t = private
                    Table_users : t
                  | Table_archived : t
                  | Query_active_users : T.Types.Text.t -> t
                  | Query_recent_users : T.Types.Int.t * T.Types.Text.t -> t
                val table_users : t
                val table_archived : t
                val query_active_users : status:T.Types.Text.t -> t
                val query_recent_users :
                  min_id:T.Types.Int.t -> excluded:T.Types.Text.t -> t
              end
            val select :
              [> `RO ] T.connection ->
              before:T.Types.Any.t ->
              src:Src.t ->
              after:T.Types.Int.t ->
              (before:T.Types.Any.t -> id:T.Types.Int.t -> unit) -> unit
            module Fold :
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c reusable.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe reusable.cmo print_impl.cmo run.ml

Values are bound in the order of the final SQL, the relation's own ones included:

  $ ./run.exe | grep -E '^===|\[SQL\]'
  === table ===
  [SQL] SELECT 'b' AS before, s.id FROM `archived` AS s WHERE s.id > 5
  === reusable query with one parameter ===
  [SQL] SELECT 'b' AS before, s.id FROM (SELECT id, status FROM users WHERE status = 'on') AS s WHERE s.id > 5
  === reusable query with two parameters ===
  [SQL] SELECT 'b' AS before, s.id FROM (SELECT id, status FROM users WHERE id > 10 AND status <> 'off') AS s WHERE s.id > 5

PostgreSQL placeholders are numbered after the relation is chosen:

  $ sqlgg -no-header -gen caml -params postgresql -dialect postgresql reusable.sql > reusable.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c reusable.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe reusable.cmo print_impl.cmo run.ml
  $ ./run.exe | grep -E '^===|PREPARE'
  === table ===
  [MOCK] PREPARE[1]: SELECT $1 AS before, s.id FROM "archived" AS s WHERE s.id > $2
  === reusable query with one parameter ===
  [MOCK] PREPARE[2]: SELECT $1 AS before, s.id FROM (SELECT id, status FROM users WHERE status = $2) AS s WHERE s.id > $3
  === reusable query with two parameters ===
  [MOCK] PREPARE[3]: SELECT $1 AS before, s.id FROM (SELECT id, status FROM users WHERE id > $2 AND status <> $3) AS s WHERE s.id > $4
