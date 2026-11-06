W: unknown function "leaked_fn" of 1 arguments, treating as untyped
module Make(T:Sqlgg_traits.M_io) =
  struct
    let find_user sqlgg__db =
      let module Sqlgg_inline =
        struct
          module _ : Sqlgg_traits.M_io = T 
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
        end in Sqlgg_inline.select_0 sqlgg__db
    let rename sqlgg__db =
      let module Sqlgg_inline =
        struct
          module _ : Sqlgg_traits.M_io = T 
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
        end in Sqlgg_inline.rename_user sqlgg__db
    let add_user sqlgg__db =
      let module Sqlgg_inline =
        struct
          module _ : Sqlgg_traits.M_io = T 
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
        end in Sqlgg_inline.insert_users_0 sqlgg__db
    let seen sqlgg__db =
      let module Sqlgg_inline =
        struct
          module _ : Sqlgg_traits.M_io = T 
          let update_users_0 db ~seen_at =
            let set_params stmt =
              let p = T.start_params stmt 1 in
              T.set_param_Datetime p seen_at; T.finish_params p in
            T.execute db
              (Sqlgg_traits.Query.make ~filename:"inline.input.ml"
                 ~sql:"UPDATE users SET seen_at = ?" ~name:"update_users_0"
                 ~kind:(let open Sqlgg_traits.Query in Update (Some "users"))
                 ()) set_params
        end in Sqlgg_inline.update_users_0 sqlgg__db
    let noted sqlgg__db =
      let module Sqlgg_inline =
        struct
          module _ : Sqlgg_traits.M_io = T 
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
        end in Sqlgg_inline.update_audit_0 sqlgg__db
    let by_state sqlgg__db =
      let module Sqlgg_inline =
        struct
          module _ : Sqlgg_traits.M_io = T 
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
        end in Sqlgg_inline.select_0 sqlgg__db
    let set_state sqlgg__db =
      let module Sqlgg_inline =
        struct
          module _ : Sqlgg_traits.M_io = T 
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
        end in Sqlgg_inline.update_posts_0 sqlgg__db
    let create_leaked =
      [%ocaml.error
        "sqlgg: [%sql] accepts SELECT, INSERT, UPDATE or DELETE; put DDL in a -schema file"]
    let from_leaked = [%ocaml.error "sqlgg: no such table leaked"]
    let create_function =
      [%ocaml.error
        "sqlgg: [%sql] accepts SELECT, INSERT, UPDATE or DELETE; put DDL in a -schema file"]
    let call_function sqlgg__db =
      let module Sqlgg_inline =
        struct
          module _ : Sqlgg_traits.M_io = T 
          let select_0 db callback =
            let invoke_callback stmt = callback ~n:(T.get_column_Any stmt 0) in
            T.select db
              (Sqlgg_traits.Query.make ~filename:"inline.input.ml"
                 ~sql:"SELECT leaked_fn(1) AS n FROM users" ~name:"select_0"
                 ~kind:(let open Sqlgg_traits.Query in Select Nat) ())
              T.no_params invoke_callback
        end in Sqlgg_inline.select_0 sqlgg__db
    let substituted =
      [%ocaml.error
        "sqlgg: [%sql] does not support the subst property: it splices runtime strings into the SQL"]
    let verbatim =
      [%ocaml.error
        "sqlgg: [%sql] does not support the noparse property: the statement must be type-checked"]
    let reusable =
      [%ocaml.error
        "sqlgg: [%sql] does not support the include property: reusable queries need a sqlgg .sql file"]
    let dynamic =
      [%ocaml.error
        "sqlgg: [%sql] does not support the dynamic_select property: dynamic selects need a sqlgg .sql file"]
    let bad_property = [%ocaml.error "sqlgg: unknown property colour"]
    let empty = [%ocaml.error "sqlgg: [%sql] needs an SQL statement"]
    let two_literals =
      [%ocaml.error
        "sqlgg: [%sql] takes exactly one string literal, e.g. [%sql {| SELECT ... |}]"]
  end
