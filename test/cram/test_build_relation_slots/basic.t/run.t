A relation slot accepts every loaded table that fits the query. Result columns
and parameters whose type depends on the chosen table become indices of
Source.t; COUNT(1) depends on nothing, so Source.t has no index.

  $ sqlgg -no-header -gen caml -params unnamed -dialect mysql basic.sql > basic.ml
  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -i basic.ml | sed -n '/module Count_from/,/module Single/p;/module Select_id/,/val select :/p;/module Count_by_id/,/source:/p'
        module Count_from :
          sig
            module Source :
              sig
                type t = private
                    Table_users : t
                  | Table_drafts : t
                  | Table_external : t
                  | Table_logs : t
                val table_users : t
                val table_drafts : t
                val table_external : t
                val table_logs : t
              end
            val select :
              [> `RO ] T.connection -> source:Source.t -> T.Types.Int.t
            module Single :
        module Select_id :
          sig
            module Source :
              sig
                type _ t = private
                    Table_users : T.Types.Int.t t
                  | Table_drafts : T.Types.Int.t option t
                  | Table_external : T.Types.Text.t t
                val table_users : T.Types.Int.t t
                val table_drafts : T.Types.Int.t option t
                val table_external : T.Types.Text.t t
              end
            val select :
        module Count_by_id :
          sig
            module Source :
              sig
                type _ t = private
                    Table_users : T.Types.Int.t t
                  | Table_drafts : T.Types.Int.t t
                  | Table_external : T.Types.Text.t t
                val table_users : T.Types.Int.t t
                val table_drafts : T.Types.Int.t t
                val table_external : T.Types.Text.t t
              end
            val select :
              [> `RO ] T.connection ->
              source:'a Source.t -> id:'a -> T.Types.Int.t
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c basic.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe basic.cmo print_impl.cmo run.ml
  $ ./run.exe | grep -E '^===|PREPARE|\[SQL\]|get_column'
  === count_from users ===
  [MOCK] PREPARE[1]: SELECT COUNT(1) AS n FROM `users` AS source
  [SQL] SELECT COUNT(1) AS n FROM `users` AS source
  [MOCK] get_column_Int[0] = 3
  === count_from logs ===
  [MOCK] PREPARE[2]: SELECT COUNT(1) AS n FROM `logs` AS source
  [SQL] SELECT COUNT(1) AS n FROM `logs` AS source
  [MOCK] get_column_Int[0] = 4
  === select_id drafts: nullable id ===
  [MOCK] PREPARE[3]: SELECT source.id FROM `drafts` AS source
  [SQL] SELECT source.id FROM `drafts` AS source
  === select_id external: text id ===
  [MOCK] PREPARE[4]: SELECT source.id FROM `external` AS source
  [SQL] SELECT source.id FROM `external` AS source
  === count_by_id users ===
  [MOCK] PREPARE[5]: SELECT COUNT(1) AS n FROM `users` AS source WHERE source.id = ?
  [SQL] SELECT COUNT(1) AS n FROM `users` AS source WHERE source.id = 7
  [MOCK] get_column_Int[0] = 1
  === count_by_id external ===
  [MOCK] PREPARE[6]: SELECT COUNT(1) AS n FROM `external` AS source WHERE source.id = ?
  [SQL] SELECT COUNT(1) AS n FROM `external` AS source WHERE source.id = 'x'
  [MOCK] get_column_Int[0] = 1

The private type can be read but not constructed outside the generated module:

  $ ocamlfind ocamlc -package sqlgg.traits -I . -c forge.ml 2>&1 | grep -o 'Cannot create values of the private type.*'
  Cannot create values of the private type Q.Count_from.Source.t

A parameter takes the type of the column it is compared with:

  $ ocamlfind ocamlc -package sqlgg.traits -I . -c wrong_type.ml 2>&1 | grep -E '^Error'
  Error: This expression has type int64 but an expression was expected of type

The IO flavour compiles as well:

  $ sqlgg -no-header -gen caml_io -params unnamed -dialect mysql basic.sql > basic_io.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c basic_io.ml
