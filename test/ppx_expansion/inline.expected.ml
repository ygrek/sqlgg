W: unknown function "leaked_fn" of 1 arguments, treating as untyped
module Make(T:Sqlgg_traits.M_io) =
  struct
    let find_user sqlgg__db =
      let module Sqlgg_inline = (functor (T : Sqlgg_traits.M_io) ->
        struct
          let select_0 db ~id =
            let get_row stmt =
              ((T.get_column_Int stmt 0),
                (T.get_column_Text_nullable stmt 1)) in
            let set_params stmt =
              let p = T.start_params stmt 1 in
              T.set_param_Int p id; T.finish_params p in
            T.select_one_maybe db
              (Sqlgg_traits.Query.make ~filename:"inline.input.ml"
                 ~sql:"SELECT id, name FROM users WHERE id = ?"
                 ~name:"select_0"
                 ~kind:(let open Sqlgg_traits.Query in Select Zero_one) ())
              set_params get_row
        end)(T) in Sqlgg_inline.select_0 sqlgg__db
    let rename sqlgg__db =
      let module Sqlgg_inline = (functor (T : Sqlgg_traits.M_io) ->
        struct
          let rename_user db ~name ~id =
            let set_params stmt =
              let p = T.start_params stmt 2 in
              (match name with
               | None -> T.set_param_null p
               | Some v -> T.set_param_Text p v);
              T.set_param_Int p id;
              T.finish_params p in
            T.execute db
              (Sqlgg_traits.Query.make ~filename:"inline.input.ml"
                 ~sql:"UPDATE users SET name = ? WHERE id = ?"
                 ~name:"rename_user"
                 ~kind:(let open Sqlgg_traits.Query in Update (Some "users"))
                 ()) set_params
        end)(T) in Sqlgg_inline.rename_user sqlgg__db
    let add_user sqlgg__db =
      let module Sqlgg_inline = (functor (T : Sqlgg_traits.M_io) ->
        struct
          let insert_users_0 db ~id ~name =
            let set_params stmt =
              let p = T.start_params stmt 2 in
              T.set_param_Int p id;
              (match name with
               | None -> T.set_param_null p
               | Some v -> T.set_param_Text p v);
              T.finish_params p in
            T.execute db
              (Sqlgg_traits.Query.make ~filename:"inline.input.ml"
                 ~sql:"INSERT INTO users (id, name) VALUES (?, ?)"
                 ~name:"insert_users_0"
                 ~kind:(let open Sqlgg_traits.Query in Insert "users") ())
              set_params
        end)(T) in Sqlgg_inline.insert_users_0 sqlgg__db
    let seen sqlgg__db =
      let module Sqlgg_inline = (functor (T : Sqlgg_traits.M_io) ->
        struct
          let update_users_0 db ~seen_at =
            let set_params stmt =
              let p = T.start_params stmt 1 in
              T.set_param_Datetime p seen_at; T.finish_params p in
            T.execute db
              (Sqlgg_traits.Query.make ~filename:"inline.input.ml"
                 ~sql:"UPDATE users SET seen_at = ?" ~name:"update_users_0"
                 ~kind:(let open Sqlgg_traits.Query in Update (Some "users"))
                 ()) set_params
        end)(T) in Sqlgg_inline.update_users_0 sqlgg__db
    let noted sqlgg__db =
      let module Sqlgg_inline = (functor (T : Sqlgg_traits.M_io) ->
        struct
          let update_audit_0 db ~noted_at =
            let set_params stmt =
              let p = T.start_params stmt 1 in
              (match noted_at with
               | None -> T.set_param_null p
               | Some v -> T.set_param_Datetime p v);
              T.finish_params p in
            T.execute db
              (Sqlgg_traits.Query.make ~filename:"inline.input.ml"
                 ~sql:"UPDATE audit SET noted_at = ?" ~name:"update_audit_0"
                 ~kind:(let open Sqlgg_traits.Query in Update (Some "audit"))
                 ()) set_params
        end)(T) in Sqlgg_inline.update_audit_0 sqlgg__db
    let by_state sqlgg__db =
      let module Sqlgg_inline = (functor (T : Sqlgg_traits.M_io) ->
        struct
          module Enum_0 =
            (T.Make_enum)(struct
                            type t = [ `Draft  | `Published ]
                            let inj =
                              function
                              | "draft" -> `Draft
                              | "published" -> `Published
                              | s ->
                                  failwith
                                    (Printf.sprintf "Invalid enum value: %s"
                                       s)
                            let proj =
                              function
                              | `Draft -> "draft"
                              | `Published -> "published"
                          end)
          let select_0 db ~state callback =
            let invoke_callback stmt = callback ~id:(T.get_column_Int stmt 0) in
            let set_params stmt =
              let p = T.start_params stmt 1 in
              Enum_0.set_param p state; T.finish_params p in
            T.select db
              (Sqlgg_traits.Query.make ~filename:"inline.input.ml"
                 ~sql:"SELECT id FROM posts WHERE state = ?" ~name:"select_0"
                 ~kind:(let open Sqlgg_traits.Query in Select Nat) ())
              set_params invoke_callback
        end)(T) in Sqlgg_inline.select_0 sqlgg__db
    let set_state sqlgg__db =
      let module Sqlgg_inline = (functor (T : Sqlgg_traits.M_io) ->
        struct
          module Enum_0 =
            (T.Make_enum)(struct
                            type t = [ `Draft  | `Published ]
                            let inj =
                              function
                              | "draft" -> `Draft
                              | "published" -> `Published
                              | s ->
                                  failwith
                                    (Printf.sprintf "Invalid enum value: %s"
                                       s)
                            let proj =
                              function
                              | `Draft -> "draft"
                              | `Published -> "published"
                          end)
          let update_posts_0 db ~state ~id =
            let set_params stmt =
              let p = T.start_params stmt 2 in
              Enum_0.set_param p state;
              T.set_param_Int p id;
              T.finish_params p in
            T.execute db
              (Sqlgg_traits.Query.make ~filename:"inline.input.ml"
                 ~sql:"UPDATE posts SET state = ? WHERE id = ?"
                 ~name:"update_posts_0"
                 ~kind:(let open Sqlgg_traits.Query in Update (Some "posts"))
                 ()) set_params
        end)(T) in Sqlgg_inline.update_posts_0 sqlgg__db
    let create_leaked =
      [%ocaml.error
        "sqlgg: [%sql] accepts SELECT, INSERT, UPDATE or DELETE; put DDL in a -schema file"]
    let from_leaked = [%ocaml.error "sqlgg: no such table leaked"]
    let create_function =
      [%ocaml.error
        "sqlgg: [%sql] accepts SELECT, INSERT, UPDATE or DELETE; put DDL in a -schema file"]
    let call_function sqlgg__db =
      let module Sqlgg_inline = (functor (T : Sqlgg_traits.M_io) ->
        struct
          let select_0 db callback =
            let invoke_callback stmt = callback ~n:(T.get_column_Any stmt 0) in
            T.select db
              (Sqlgg_traits.Query.make ~filename:"inline.input.ml"
                 ~sql:"SELECT leaked_fn(1) AS n FROM users" ~name:"select_0"
                 ~kind:(let open Sqlgg_traits.Query in Select Nat) ())
              T.no_params invoke_callback
        end)(T) in Sqlgg_inline.select_0 sqlgg__db
    let substituted =
      [%ocaml.error
        "sqlgg: [%sql] does not support the subst property: it splices runtime strings into the SQL"]
    let verbatim =
      [%ocaml.error
        "sqlgg: [%sql] does not support the noparse property: the statement must be type-checked"]
    let reusable =
      [%ocaml.error
        "sqlgg: [%sql] does not support the include property: include modes belong in a sqlgg .sql file"]
    let executable =
      [%ocaml.error
        "sqlgg: [%sql] does not support the include property: include modes belong in a sqlgg .sql file"]
    let dynamic =
      [%ocaml.error
        "sqlgg: [%sql] does not support the dynamic_select property: dynamic select modes belong in a sqlgg .sql file"]
    let static =
      [%ocaml.error
        "sqlgg: [%sql] does not support the dynamic_select property: dynamic select modes belong in a sqlgg .sql file"]
    let migration =
      [%ocaml.error
        "sqlgg: [%sql] does not support the id property: migration properties belong in a sqlgg .sql file"]
    let bad_property = [%ocaml.error "sqlgg: unknown property colour"]
    let empty = [%ocaml.error "sqlgg: [%sql] needs an SQL statement"]
    let two_literals =
      [%ocaml.error
        "sqlgg: [%sql] takes exactly one string literal, e.g. [%sql {| SELECT ... |}]"]
  end
module With_database(Database:Sqlgg_traits.M_io) =
  struct
    let find_user sqlgg__db =
      let module Sqlgg_inline = (functor (T : Sqlgg_traits.M_io) ->
        struct
          let select_0 db ~id =
            let get_row stmt =
              ((T.get_column_Int stmt 0),
                (T.get_column_Text_nullable stmt 1)) in
            let set_params stmt =
              let p = T.start_params stmt 1 in
              T.set_param_Int p id; T.finish_params p in
            T.select_one_maybe db
              (Sqlgg_traits.Query.make ~filename:"inline.input.ml"
                 ~sql:"SELECT id, name FROM users WHERE id = ?"
                 ~name:"select_0"
                 ~kind:(let open Sqlgg_traits.Query in Select Zero_one) ())
              set_params get_row
        end)(Database) in Sqlgg_inline.select_0 sqlgg__db
  end
module With_nested_database(Runtime:sig module Database : Sqlgg_traits.M_io
                                    end) =
  struct
    let find_user sqlgg__db =
      let module Sqlgg_inline = (functor (T : Sqlgg_traits.M_io) ->
        struct
          let select_0 db ~id =
            let get_row stmt =
              ((T.get_column_Int stmt 0),
                (T.get_column_Text_nullable stmt 1)) in
            let set_params stmt =
              let p = T.start_params stmt 1 in
              T.set_param_Int p id; T.finish_params p in
            T.select_one_maybe db
              (Sqlgg_traits.Query.make ~filename:"inline.input.ml"
                 ~sql:"SELECT id, name FROM users WHERE id = ?"
                 ~name:"select_0"
                 ~kind:(let open Sqlgg_traits.Query in Select Zero_one) ())
              set_params get_row
        end)(Runtime.Database) in Sqlgg_inline.select_0 sqlgg__db
  end
let find_user_request ~id =
  ({
     Sqlgg_request.sql =
       (fun _ -> Sqlgg_request.Sql "SELECT id, name FROM users WHERE id = ?");
     name = "select_0";
     kind = (Sqlgg_query.Select Sqlgg_query.Zero_one);
     params =
       (fun sqlgg__binder ->
          sqlgg__binder.Sqlgg_request.bind
            (Sqlgg_request.Required Sqlgg_request.Int) id);
     result =
       (Sqlgg_request.Column
          ((Sqlgg_request.Column
              ((Sqlgg_request.Build (fun x1 x2 -> (x1, x2))),
                (Sqlgg_request.Required Sqlgg_request.Int))),
            (Sqlgg_request.Optional Sqlgg_request.Text)))
   } : (_, Sqlgg_request.zero_or_one) Sqlgg_request.t)
let rename_user_request ~name ~id =
  ({
     Sqlgg_request.sql =
       (fun _ -> Sqlgg_request.Sql "UPDATE users SET name = ? WHERE id = ?");
     name = "update_users_0";
     kind = (Sqlgg_query.Update (Some "users"));
     params =
       (fun sqlgg__binder ->
          sqlgg__binder.Sqlgg_request.bind
            (Sqlgg_request.Optional Sqlgg_request.Text) name;
          sqlgg__binder.Sqlgg_request.bind
            (Sqlgg_request.Required Sqlgg_request.Int) id);
     result = (Sqlgg_request.Build ())
   } : (_, Sqlgg_request.affected_rows) Sqlgg_request.t)
let in_list_request ~ids =
  ({
     Sqlgg_request.sql =
       (fun _sqlgg__literals ->
          Sqlgg_request.Sql
            ("SELECT id FROM users WHERE " ^
               (match ids with
                | [] -> "FALSE"
                | _::_ ->
                    "id IN " ^
                      ("(" ^
                         ((String.concat ", "
                             (List.map
                                (_sqlgg__literals.Sqlgg_request.literal
                                   (Sqlgg_request.Required Sqlgg_request.Int))
                                ids))
                            ^ ")")))));
     name = "select_0";
     kind = (Sqlgg_query.Select Sqlgg_query.Nat);
     params = (fun _ -> ());
     result =
       (Sqlgg_request.Column
          ((Sqlgg_request.Build (fun x1 -> x1)),
            (Sqlgg_request.Required Sqlgg_request.Int)))
   } : (_, Sqlgg_request.many) Sqlgg_request.t)
let value_rows_request ~rows =
  ({
     Sqlgg_request.sql =
       (fun _sqlgg__literals ->
          match rows with
          | [] -> Sqlgg_request.Nothing_to_run
          | _::_ ->
              Sqlgg_request.Sql
                ("INSERT INTO users (id, name) VALUES " ^
                   (let _sqlgg_b = Buffer.create 13 in
                    (List.iteri
                       (fun _sqlgg_idx (id, name) ->
                          Buffer.add_string _sqlgg_b
                            (if _sqlgg_idx = 0 then "(" else ", (");
                          Buffer.add_string _sqlgg_b
                            ((_sqlgg__literals.Sqlgg_request.literal
                                (Sqlgg_request.Required Sqlgg_request.Int))
                               id);
                          Buffer.add_string _sqlgg_b ", ";
                          Buffer.add_string _sqlgg_b
                            (match name with
                             | None -> "NULL"
                             | Some v ->
                                 (_sqlgg__literals.Sqlgg_request.literal
                                    (Sqlgg_request.Required
                                       Sqlgg_request.Text)) v);
                          Buffer.add_char _sqlgg_b ')') rows;
                     Buffer.contents _sqlgg_b))));
     name = "insert_users_0";
     kind = (Sqlgg_query.Insert "users");
     params = (fun _ -> ());
     result = (Sqlgg_request.Build ())
   } : (_, Sqlgg_request.affected_rows) Sqlgg_request.t)
let tuple_in_request ~pairs =
  ({
     Sqlgg_request.sql =
       (fun _sqlgg__literals ->
          Sqlgg_request.Sql
            ("SELECT id FROM users WHERE " ^
               (match pairs with
                | [] -> "FALSE"
                | _::_ ->
                    "(id, name) IN " ^
                      ("(" ^
                         ((let _sqlgg_b = Buffer.create 13 in
                           List.iteri
                             (fun _sqlgg_idx (pairs_0n, pairs_1n) ->
                                Buffer.add_string _sqlgg_b
                                  (if _sqlgg_idx = 0 then "(" else ", (");
                                Buffer.add_string _sqlgg_b
                                  ((_sqlgg__literals.Sqlgg_request.literal
                                      (Sqlgg_request.Required
                                         Sqlgg_request.Int)) pairs_0n);
                                Buffer.add_string _sqlgg_b ", ";
                                Buffer.add_string _sqlgg_b
                                  ((_sqlgg__literals.Sqlgg_request.literal
                                      (Sqlgg_request.Required
                                         Sqlgg_request.Text)) pairs_1n);
                                Buffer.add_char _sqlgg_b ')') pairs;
                           Buffer.contents _sqlgg_b) ^ ")")))));
     name = "select_0";
     kind = (Sqlgg_query.Select Sqlgg_query.Nat);
     params = (fun _ -> ());
     result =
       (Sqlgg_request.Column
          ((Sqlgg_request.Build (fun x1 -> x1)),
            (Sqlgg_request.Required Sqlgg_request.Int)))
   } : (_, Sqlgg_request.many) Sqlgg_request.t)
let choice_request ~filter =
  ({
     Sqlgg_request.sql =
       (fun _sqlgg__literals ->
          Sqlgg_request.Sql
            ("SELECT id FROM users WHERE " ^
               (match filter with
                | `By_id _ -> " ( id = ? ) "
                | `By_name _ -> " ( name = ? ) ")));
     name = "select_0";
     kind = (Sqlgg_query.Select Sqlgg_query.Nat);
     params =
       (fun sqlgg__binder ->
          match filter with
          | `By_id id ->
              sqlgg__binder.Sqlgg_request.bind
                (Sqlgg_request.Required Sqlgg_request.Int) id
          | `By_name name ->
              sqlgg__binder.Sqlgg_request.bind
                (Sqlgg_request.Required Sqlgg_request.Text) name);
     result =
       (Sqlgg_request.Column
          ((Sqlgg_request.Build (fun x1 -> x1)),
            (Sqlgg_request.Required Sqlgg_request.Int)))
   } : (_, Sqlgg_request.many) Sqlgg_request.t)
let option_request ~min ~name_filter =
  ({
     Sqlgg_request.sql =
       (fun _sqlgg__literals ->
          Sqlgg_request.Sql
            ("SELECT id FROM users WHERE id > ? AND " ^
               (match name_filter with
                | `Some _ -> " ( name = ? ) "
                | `None -> " ( TRUE ) ")));
     name = "select_0";
     kind = (Sqlgg_query.Select Sqlgg_query.Nat);
     params =
       (fun sqlgg__binder ->
          sqlgg__binder.Sqlgg_request.bind
            (Sqlgg_request.Required Sqlgg_request.Int) min;
          (match name_filter with
           | `None -> ()
           | `Some name ->
               sqlgg__binder.Sqlgg_request.bind
                 (Sqlgg_request.Required Sqlgg_request.Text) name));
     result =
       (Sqlgg_request.Column
          ((Sqlgg_request.Build (fun x1 -> x1)),
            (Sqlgg_request.Required Sqlgg_request.Int)))
   } : (_, Sqlgg_request.many) Sqlgg_request.t)
let count_users_request =
  ({
     Sqlgg_request.sql =
       (fun _ -> Sqlgg_request.Sql "SELECT COUNT(*) FROM users");
     name = "select_0";
     kind = (Sqlgg_query.Select Sqlgg_query.One);
     params = (fun _ -> ());
     result =
       (Sqlgg_request.Column
          ((Sqlgg_request.Build (fun x1 -> x1)),
            (Sqlgg_request.Required Sqlgg_request.Int)))
   } : (_, Sqlgg_request.exactly_one) Sqlgg_request.t)
let all_ids_request =
  ({
     Sqlgg_request.sql = (fun _ -> Sqlgg_request.Sql "SELECT id FROM users");
     name = "select_0";
     kind = (Sqlgg_query.Select Sqlgg_query.Nat);
     params = (fun _ -> ());
     result =
       (Sqlgg_request.Column
          ((Sqlgg_request.Build (fun x1 -> x1)),
            (Sqlgg_request.Required Sqlgg_request.Int)))
   } : (_, Sqlgg_request.many) Sqlgg_request.t)
let datetime_request =
  ({
     Sqlgg_request.sql =
       (fun _ -> Sqlgg_request.Sql "SELECT seen_at FROM users");
     name = "select_0";
     kind = (Sqlgg_query.Select Sqlgg_query.Nat);
     params = (fun _ -> ());
     result =
       (Sqlgg_request.Column
          ((Sqlgg_request.Build (fun x1 -> x1)),
            (Sqlgg_request.Optional Sqlgg_request.Datetime)))
   } : (_, Sqlgg_request.many) Sqlgg_request.t)
let state_request ~id =
  ({
     Sqlgg_request.sql =
       (fun _ -> Sqlgg_request.Sql "SELECT state FROM posts WHERE id = ?");
     name = "select_0";
     kind = (Sqlgg_query.Select Sqlgg_query.Zero_one);
     params =
       (fun sqlgg__binder ->
          sqlgg__binder.Sqlgg_request.bind
            (Sqlgg_request.Required Sqlgg_request.Int) id);
     result =
       (Sqlgg_request.Column
          ((Sqlgg_request.Build (fun x1 -> x1)),
            (Sqlgg_request.Required
               (Sqlgg_request.Enum ((module
                  struct
                    type t = [ `Draft  | `Published ]
                    let inj =
                      function
                      | "draft" -> `Draft
                      | "published" -> `Published
                      | value ->
                          failwith
                            (Printf.sprintf "Invalid enum value: %s" value)
                    let proj =
                      function
                      | `Draft -> "draft"
                      | `Published -> "published"
                  end) : (module Sqlgg_traits.Enum with type t =
                           [ `Draft  | `Published ]))))))
   } : (_, Sqlgg_request.zero_or_one) Sqlgg_request.t)
let json_path_request ~mode ~path =
  ({
     Sqlgg_request.sql =
       (fun _ ->
          Sqlgg_request.Sql
            "SELECT id FROM docs WHERE JSON_CONTAINS_PATH(body, ?, ?)");
     name = "select_0";
     kind = (Sqlgg_query.Select Sqlgg_query.Nat);
     params =
       (fun sqlgg__binder ->
          sqlgg__binder.Sqlgg_request.bind
            (Sqlgg_request.Required Sqlgg_request.One_or_all) mode;
          sqlgg__binder.Sqlgg_request.bind
            (Sqlgg_request.Required Sqlgg_request.Json_path) path);
     result =
       (Sqlgg_request.Column
          ((Sqlgg_request.Build (fun x1 -> x1)),
            (Sqlgg_request.Required Sqlgg_request.Int)))
   } : (_, Sqlgg_request.many) Sqlgg_request.t)
let three_columns_request ~id =
  ({
     Sqlgg_request.sql =
       (fun _ ->
          Sqlgg_request.Sql
            "SELECT id, name, id AS again FROM users WHERE id = ?");
     name = "select_0";
     kind = (Sqlgg_query.Select Sqlgg_query.Zero_one);
     params =
       (fun sqlgg__binder ->
          sqlgg__binder.Sqlgg_request.bind
            (Sqlgg_request.Required Sqlgg_request.Int) id);
     result =
       (Sqlgg_request.Column
          ((Sqlgg_request.Column
              ((Sqlgg_request.Column
                  ((Sqlgg_request.Build (fun x1 x2 x3 -> (x1, x2, x3))),
                    (Sqlgg_request.Required Sqlgg_request.Int))),
                (Sqlgg_request.Optional Sqlgg_request.Text))),
            (Sqlgg_request.Required Sqlgg_request.Int)))
   } : (_, Sqlgg_request.zero_or_one) Sqlgg_request.t)
