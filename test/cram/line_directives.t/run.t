`-line-directives` makes the generated OCaml carry `# <line> "<file.sql>"`
directives, so that ocamlc and merlin report an error inside a generated query
function on the line of the query it came from, instead of on a line of the
machine-written .ml that nobody reads.

Start with the smallest possible file, and look at the whole thing.

  $ grep -n '' one.sql
  1:CREATE TABLE t (id INT NOT NULL);
  2:
  3:-- @erase
  4:DELETE FROM t WHERE id = @id;

Off by default, the output is exactly what it always was:

  $ sqlgg -no-header -gen caml one.sql
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_t db  =
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"one.sql" ~sql:("CREATE TABLE t (id INT NOT NULL)") ~name:"create_t" ~kind:Sqlgg_traits.Query.(Create "t") ())
  
    let erase db ~id =
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p id;
        T.finish_params p
      in
      T.execute db (Sqlgg_traits.Query.make ~filename:"one.sql" ~sql:("DELETE FROM t WHERE id = @id") ~name:"erase" ~kind:Sqlgg_traits.Query.(Delete ["t"]) ()) set_params
  
  end (* module Sqlgg *)

On, each generated line carries the line its query starts at -- 1 for the
CREATE, 4 for the DELETE, which is the statement itself and not the `-- @erase`
comment on line 3. Leaving a function, a second directive hands numbering back
to the generated file, so what follows is attributed to it and not to whatever
comes next in the .sql:

  $ sqlgg -no-header -gen caml -line-directives one.sql
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
  # 1 "one.sql"
    let create_t db  =
  # 1 "one.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"one.sql" ~sql:("CREATE TABLE t (id INT NOT NULL)") ~name:"create_t" ~kind:Sqlgg_traits.Query.(Create "t") ())
  # 10 "one.sql.ml"
  
  # 4 "one.sql"
    let erase db ~id =
  # 4 "one.sql"
      let set_params stmt =
  # 4 "one.sql"
        let p = T.start_params stmt (1) in
  # 4 "one.sql"
        T.set_param_Int p id;
  # 4 "one.sql"
        T.finish_params p
  # 4 "one.sql"
      in
  # 4 "one.sql"
      T.execute db (Sqlgg_traits.Query.make ~filename:"one.sql" ~sql:("DELETE FROM t WHERE id = @id") ~name:"erase" ~kind:Sqlgg_traits.Query.(Delete ["t"]) ()) set_params
  # 26 "one.sql.ml"
  
  end (* module Sqlgg *)

No generator other than caml/caml_io emits anything, with or without the flag:

  $ for g in cxx java xml csharp; do
  >   sqlgg -no-header -line-directives -gen $g one.sql | grep -c '^# ' || true
  > done
  0
  0
  0
  0

Reading from stdin there is no file to point at, so nothing is emitted either:

  $ sqlgg -no-header -gen caml -line-directives - < one.sql | grep -c '^# ' || true
  0

`-line-directives-file` names the generated file explicitly, for build rules
whose target is not `<input>.ml`:

  $ sqlgg -no-header -gen caml -line-directives -line-directives-file sql_t.ml one.sql
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
  # 1 "one.sql"
    let create_t db  =
  # 1 "one.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"one.sql" ~sql:("CREATE TABLE t (id INT NOT NULL)") ~name:"create_t" ~kind:Sqlgg_traits.Query.(Create "t") ())
  # 10 "sql_t.ml"
  
  # 4 "one.sql"
    let erase db ~id =
  # 4 "one.sql"
      let set_params stmt =
  # 4 "one.sql"
        let p = T.start_params stmt (1) in
  # 4 "one.sql"
        T.set_param_Int p id;
  # 4 "one.sql"
        T.finish_params p
  # 4 "one.sql"
      in
  # 4 "one.sql"
      T.execute db (Sqlgg_traits.Query.make ~filename:"one.sql" ~sql:("DELETE FROM t WHERE id = @id") ~name:"erase" ~kind:Sqlgg_traits.Query.(Delete ["t"]) ()) set_params
  # 26 "sql_t.ml"
  
  end (* module Sqlgg *)

Now a file with a multi-line query and several module kinds. Note that the SQL
stays one OCaml string literal with escaped newlines and no directive spliced
into it, and that `module Single = struct` / `end (* module Single *)` are not
preceded by one either:

  $ grep -n '' queries.sql
  1:CREATE TABLE person (
  2:  id INT NOT NULL,
  3:  name TEXT NOT NULL
  4:);
  5:
  6:-- @count_persons
  7:SELECT count(*) FROM person;
  8:
  9:-- @rename
  10:UPDATE person
  11:   SET name = @name
  12: WHERE id = @id;
  13:
  14:-- @erase
  15:DELETE FROM person WHERE id = @id;
  $ sqlgg -no-header -gen caml -line-directives queries.sql > out.ml
  $ cat out.ml
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
  # 1 "queries.sql"
    let create_person db  =
  # 1 "queries.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("CREATE TABLE person (\n\
    id INT NOT NULL,\n\
    name TEXT NOT NULL\n\
  )") ~name:"create_person" ~kind:Sqlgg_traits.Query.(Create "person") ())
  # 13 "queries.sql.ml"
  
  # 7 "queries.sql"
    let count_persons db  =
  # 7 "queries.sql"
      let get_row stmt =
  # 7 "queries.sql"
        (T.get_column_Int stmt 0)
  # 7 "queries.sql"
      in
  # 7 "queries.sql"
      T.select_one db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("SELECT count(*) FROM person") ~name:"count_persons" ~kind:Sqlgg_traits.Query.(Select One) ()) T.no_params get_row
  # 25 "queries.sql.ml"
  
  # 10 "queries.sql"
    let rename db ~name ~id =
  # 10 "queries.sql"
      let set_params stmt =
  # 10 "queries.sql"
        let p = T.start_params stmt (2) in
  # 10 "queries.sql"
        T.set_param_Text p name;
  # 10 "queries.sql"
        T.set_param_Int p id;
  # 10 "queries.sql"
        T.finish_params p
  # 10 "queries.sql"
      in
  # 10 "queries.sql"
      T.execute db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("UPDATE person\n\
     SET name = @name\n\
   WHERE id = @id") ~name:"rename" ~kind:Sqlgg_traits.Query.(Update (Some "person")) ()) set_params
  # 45 "queries.sql.ml"
  
  # 15 "queries.sql"
    let erase db ~id =
  # 15 "queries.sql"
      let set_params stmt =
  # 15 "queries.sql"
        let p = T.start_params stmt (1) in
  # 15 "queries.sql"
        T.set_param_Int p id;
  # 15 "queries.sql"
        T.finish_params p
  # 15 "queries.sql"
      in
  # 15 "queries.sql"
      T.execute db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("DELETE FROM person WHERE id = @id") ~name:"erase" ~kind:Sqlgg_traits.Query.(Delete ["person"]) ()) set_params
  # 61 "queries.sql.ml"
  
    module Single = struct
  # 7 "queries.sql"
      let count_persons db  callback =
  # 7 "queries.sql"
        let invoke_callback stmt =
  # 7 "queries.sql"
          callback
  # 7 "queries.sql"
            ~r:(T.get_column_Int stmt 0)
  # 7 "queries.sql"
        in
  # 7 "queries.sql"
        T.select_one db (Sqlgg_traits.Query.make ~filename:"queries.sql" ~sql:("SELECT count(*) FROM person") ~name:"count_persons" ~kind:Sqlgg_traits.Query.(Select One) ()) T.no_params invoke_callback
  # 76 "queries.sql.ml"
  
    end (* module Single *)
  end (* module Sqlgg *)
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -c out.ml
  $ echo $?
  0

Dynamic selects are generated through their own path -- a per-query column
module plus its `select` -- and get the same treatment:

  $ grep -n '' dyn.sql
  1:CREATE TABLE items (id INT NOT NULL PRIMARY KEY, name TEXT NULL);
  2:
  3:-- [sqlgg] dynamic_select=true
  4:-- @pick
  5:SELECT id, name
  6:  FROM items
  7: WHERE id = @id;
  $ sqlgg -no-header -gen caml -line-directives dyn.sql > dyn.ml
  $ cat dyn.ml
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Pick = struct
      type brand
      include Sqlgg_scope.Make (struct type nonrec brand = brand type row = T.row type params = T.params end)
      module Cols = struct
  # 5 "dyn.sql"
        let id : _ t =
  # 5 "dyn.sql"
          {
  # 5 "dyn.sql"
            set = (fun _p -> ());
  # 5 "dyn.sql"
            read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
  # 5 "dyn.sql"
            column = ("id");
  # 5 "dyn.sql"
            count = 0;
  # 5 "dyn.sql"
            deps = [];
  # 5 "dyn.sql"
          }
  # 25 "dyn.sql.ml"
  # 5 "dyn.sql"
        let name : _ t =
  # 5 "dyn.sql"
          {
  # 5 "dyn.sql"
            set = (fun _p -> ());
  # 5 "dyn.sql"
            read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
  # 5 "dyn.sql"
            column = ("name");
  # 5 "dyn.sql"
            count = 0;
  # 5 "dyn.sql"
            deps = [];
  # 5 "dyn.sql"
          }
  # 42 "dyn.sql.ml"
      end
      include Cols
      let cols = object
        method id = Cols.id
        method name = Cols.name
      end
  
  # 5 "dyn.sql"
      let select db (col : _ t) ~id =
  # 5 "dyn.sql"
        let set_params stmt =
  # 5 "dyn.sql"
          let p = T.start_params stmt (1 + col.count) in
  # 5 "dyn.sql"
          col.set p;
  # 5 "dyn.sql"
          T.set_param_Int p id;
  # 5 "dyn.sql"
          T.finish_params p
  # 5 "dyn.sql"
        in
  # 5 "dyn.sql"
        T.select_one_maybe db
  # 5 "dyn.sql"
        (Sqlgg_traits.Query.make ~filename:"dyn.sql" ~sql:("SELECT " ^ col.column ^ "\n\
    FROM items\n\
   WHERE id = @id") ~name:"pick" ~kind:Sqlgg_traits.Query.(Select Zero_one) ())
  # 5 "dyn.sql"
        set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col))
  # 72 "dyn.sql.ml"
  
    end
  
  
  # 1 "dyn.sql"
    let create_items db  =
  # 1 "dyn.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"dyn.sql" ~sql:("CREATE TABLE items (id INT NOT NULL PRIMARY KEY, name TEXT NULL)") ~name:"create_items" ~kind:Sqlgg_traits.Query.(Create "items") ())
  # 81 "dyn.sql.ml"
  
    module Single = struct
    end (* module Single *)
  end (* module Sqlgg *)
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -c dyn.ml
  $ echo $?
  0

The mapping has to hold for the whole function body, which is much longer than
the query it came from. Here `big` starts on line 9 and `after` already on 15,
while the body of `big` runs for a good deal more than six lines. Column `bad`
carries a `module=` that does not exist and is bound by the last parameter, so
the broken line sits at the very end of that body:

  $ grep -n '' long.sql
  1:CREATE TABLE wide (
  2:  c1 INT NOT NULL, c2 INT NOT NULL, c3 INT NOT NULL, c4 INT NOT NULL,
  3:  c5 INT NOT NULL, c6 INT NOT NULL, c7 INT NOT NULL, c8 INT NOT NULL,
  4:  -- [sqlgg] module=No_such_codec
  5:  bad INT NOT NULL
  6:);
  7:
  8:-- @big
  9:UPDATE wide
  10:   SET c1 = @p1, c2 = @p2, c3 = @p3, c4 = @p4,
  11:       c5 = @p5, c6 = @p6, c7 = @p7, c8 = @p8
  12: WHERE bad = @bad;
  13:
  14:-- @after
  15:DELETE FROM wide;
  $ sqlgg -no-header -gen caml -line-directives long.sql > long.ml
  $ cat long.ml
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
  # 1 "long.sql"
    let create_wide db  =
  # 1 "long.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"long.sql" ~sql:("CREATE TABLE wide (\n\
    c1 INT NOT NULL, c2 INT NOT NULL, c3 INT NOT NULL, c4 INT NOT NULL,\n\
    c5 INT NOT NULL, c6 INT NOT NULL, c7 INT NOT NULL, c8 INT NOT NULL,\n\
      bad INT NOT NULL\n\
  )") ~name:"create_wide" ~kind:Sqlgg_traits.Query.(Create "wide") ())
  # 14 "long.sql.ml"
  
  # 9 "long.sql"
    let big db ~p1 ~p2 ~p3 ~p4 ~p5 ~p6 ~p7 ~p8 ~bad =
  # 9 "long.sql"
      let set_params stmt =
  # 9 "long.sql"
        let p = T.start_params stmt (9) in
  # 9 "long.sql"
        T.set_param_Int p p1;
  # 9 "long.sql"
        T.set_param_Int p p2;
  # 9 "long.sql"
        T.set_param_Int p p3;
  # 9 "long.sql"
        T.set_param_Int p p4;
  # 9 "long.sql"
        T.set_param_Int p p5;
  # 9 "long.sql"
        T.set_param_Int p p6;
  # 9 "long.sql"
        T.set_param_Int p p7;
  # 9 "long.sql"
        T.set_param_Int p p8;
  # 9 "long.sql"
        T.set_param_int64 p (No_such_codec.set_param bad);
  # 9 "long.sql"
        T.finish_params p
  # 9 "long.sql"
      in
  # 9 "long.sql"
      T.execute db (Sqlgg_traits.Query.make ~filename:"long.sql" ~sql:("UPDATE wide\n\
     SET c1 = @p1, c2 = @p2, c3 = @p3, c4 = @p4,\n\
         c5 = @p5, c6 = @p6, c7 = @p7, c8 = @p8\n\
   WHERE bad = @bad") ~name:"big" ~kind:Sqlgg_traits.Query.(Update (Some "wide")) ()) set_params
  # 49 "long.sql.ml"
  
  # 15 "long.sql"
    let after db  =
  # 15 "long.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~filename:"long.sql" ~sql:("DELETE FROM wide") ~name:"after" ~kind:Sqlgg_traits.Query.(Delete ["wide"]) ())
  # 55 "long.sql.ml"
  
  end (* module Sqlgg *)

A single directive per function would have numbered the broken line past the end
of long.sql; with a shorter body it would have landed on `after`. Re-pinning
every line keeps it on 9:

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -c long.ml 2>&1
  File "long.sql", line 9, characters 27-50:
  Error: Unbound module No_such_codec
  [2]

Without the flag the same error is reported against the generated file, as before:

  $ sqlgg -no-header -gen caml long.sql > long_plain.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -c long_plain.ml 2>&1
  File "long_plain.ml", line 23, characters 27-50:
  23 |       T.set_param_int64 p (No_such_codec.set_param bad);
                                  ^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unbound module No_such_codec
  [2]

Migrations: `apply_*` and `revert_*` come from two separate SQL blocks and are
mapped onto their own, here lines 3 and 5 of extends.sql:

  $ grep -n '' extends.sql
  1:-- [sqlgg] manual
  2:-- [sqlgg] id=20260609000000
  3:ALTER TABLE users
  4:  RENAME COLUMN email TO email_address;
  5:ALTER TABLE users
  6:  RENAME COLUMN email_address TO email;
  $ sqlgg -no-header -dialect mysql -migrate -now 20260101000000 -gen caml -name migrations \
  >   -line-directives -initial initial.sql -extends extends.sql -target target.sql
  module Migrations (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
  # 3 "extends.sql"
    let apply_alter_users_0 db  =
  # 3 "extends.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("ALTER TABLE users\n\
    RENAME COLUMN email TO email_address") ~name:"apply_alter_users_0" ~kind:Sqlgg_traits.Query.Other ())
  # 11 "extends.sql.ml"
  
  # 5 "extends.sql"
    let revert_alter_users_0 db  =
  # 5 "extends.sql"
      T.execute_unprepared db (Sqlgg_traits.Query.make ~sql:("ALTER TABLE users\n\
    RENAME COLUMN email_address TO email") ~name:"revert_alter_users_0" ~kind:Sqlgg_traits.Query.Other ())
  # 18 "extends.sql.ml"
  
    let migrations = [
      ("alter_users_0", apply_alter_users_0, revert_alter_users_0);
    ]
  
  end (* module Migrations *)
  nothing new to migrate; regenerated code from 1 recorded migration(s)
