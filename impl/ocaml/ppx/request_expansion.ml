open Ppxlib

let fail ~statement fmt =
  Location.raise_errorf ~loc:statement ("sqlgg: " ^^ fmt)

let expand ~loc ~statement (query : Query.named) =
  let ty ~codec ~meta (typ : Sqlgg.Sql.Type.t) =
    match codec meta with
    | Some _ -> fail ~statement "[%%sql.Request] does not support custom codecs"
    | None ->
      let scalar =
        match typ.t with
        | Bool -> [%expr Sqlgg_request.Bool]
        | Int -> [%expr Sqlgg_request.Int]
        | UInt64 -> [%expr Sqlgg_request.UInt64]
        | Float | FloatingLiteral _ -> [%expr Sqlgg_request.Float]
        | Decimal _ -> [%expr Sqlgg_request.Decimal]
        | Text | StringLiteral _ -> [%expr Sqlgg_request.Text]
        | Blob -> [%expr Sqlgg_request.Blob]
        | Datetime -> [%expr Sqlgg_request.Datetime]
        | Json -> [%expr Sqlgg_request.Json]
        | Json_path -> [%expr Sqlgg_request.Json_path]
        | One_or_all -> [%expr Sqlgg_request.One_or_all]
        | Union { ctors; _ } ->
          let open Ast_builder.Default in
          let ctors =
            List.map
              (fun value -> value, Gen_caml.sanitize_to_variant_name value)
              (Sqlgg.Sql.Type.Enum_kind.Ctors.elements ctors)
          in
          let variant =
            ptyp_variant ~loc
              (List.map (fun (_, name) -> rtag ~loc (Located.mk ~loc name) true []) ctors)
              Closed None
          in
          [%expr
            Sqlgg_request.Enum
              (module struct
                type t = [%t variant]

                let inj =
                  [%e
                    pexp_function_cases ~loc
                      (List.fold_right
                         (fun (value, name) cases ->
                           case ~lhs:(pstring ~loc value) ~guard:None
                             ~rhs:(pexp_variant ~loc name None)
                           :: cases)
                         ctors
                         [ case ~lhs:(pvar ~loc "value") ~guard:None
                             ~rhs:[%expr failwith (Printf.sprintf "Invalid enum value: %s" value)] ])]

                let proj =
                  [%e
                    pexp_function_cases ~loc
                      (List.map
                         (fun (value, name) ->
                           case ~lhs:(ppat_variant ~loc name None) ~guard:None
                             ~rhs:(estring ~loc value))
                         ctors)]
              end : Sqlgg_traits.Enum with type t = [%t variant])]
        | Any ->
          fail ~statement "[%%sql.Request] does not support type %s"
            (Sqlgg.Sql.Type.show typ)
      in
      if Sqlgg.Sql.Type.is_nullable typ then [%expr Sqlgg_request.Optional [%e scalar]]
      else [%expr Sqlgg_request.Required [%e scalar]]
  in
  let param_ty ~meta typ =
    Pprintast.string_of_expression
      (ty ~codec:Gen_caml.codec_set_param ~meta typ)
  in
  let parsed text =
    (object
      inherit Ast_traverse.map
      method! location _ = loc
    end)#expression
      (Parse.expression (Lexing.from_string text))
  in
  let sql =
    match Gen.get_sql query with
    | [ `Text text ] ->
      [%expr fun _ -> Sqlgg_request.Sql [%e Ast_builder.Default.estring ~loc text]]
    | pieces ->
      let to_literal meta (typ : Sqlgg.Sql.Type.t) =
        Printf.sprintf "(_sqlgg__literals.Sqlgg_request.literal (%s))"
          (param_ty ~meta (Sqlgg.Sql.Type.make_strict typ))
      in
      let outcome =
        Gen_caml.guard_empty_insertion pieces ~empty:"Sqlgg_request.Nothing_to_run"
          (Printf.sprintf "Sqlgg_request.Sql %s"
             (Gen_caml.make_sql ~to_literal ~join_ctors:[] pieces))
      in
      [%expr fun _sqlgg__literals -> [%e parsed outcome]]
  in
  let kind, cardinality =
    let table_name table =
      Ast_builder.Default.estring ~loc (Sqlgg.Sql.show_table_name table)
    in
    match query.stmt.kind with
    | Select `Zero_one ->
      [%expr Sqlgg_query.Select Sqlgg_query.Zero_one],
      [%type: Sqlgg_request.zero_or_one]
    | Select `One ->
      [%expr Sqlgg_query.Select Sqlgg_query.One],
      [%type: Sqlgg_request.exactly_one]
    | Select `Nat ->
      [%expr Sqlgg_query.Select Sqlgg_query.Nat],
      [%type: Sqlgg_request.many]
    | Insert (_, table) ->
      [%expr Sqlgg_query.Insert [%e table_name table]],
      [%type: Sqlgg_request.affected_rows]
    | Update (Some table) ->
      [%expr Sqlgg_query.Update (Some [%e table_name table])],
      [%type: Sqlgg_request.affected_rows]
    | Update None ->
      [%expr Sqlgg_query.Update None],
      [%type: Sqlgg_request.affected_rows]
    | Delete tables ->
      [%expr
        Sqlgg_query.Delete
          [%e Ast_builder.Default.elist ~loc (List.map table_name tables)]],
      [%type: Sqlgg_request.affected_rows]
    | Create _ | CreateIndex _ | Alter _ | Drop _ | CreateRoutine _
    | CreateType _ | DropType _ | Other ->
      fail ~statement "[%%sql.Request] accepts SELECT, INSERT, UPDATE or DELETE"
  in
  let params =
    let bound = Gen_caml.exclude_in_vars query.stmt.vars in
    if not (Gen_caml.has_set_params bound) then [%expr fun _ -> ()]
    else
      let bind ~meta index (param : Sqlgg.Sql.Type.t Sqlgg.Sql.param) =
        Gen.output "sqlgg__binder.Sqlgg_request.bind (%s) %s;"
          (param_ty ~meta param.typ)
          (Gen.show_param_name param index)
      in
      [%expr
        fun sqlgg__binder ->
          [%e
            parsed
              (Gen.capture (fun () -> List.iteri (Gen_caml.set_var ~leaf:bind) bound))]]
  in
  let result =
    let open Ast_builder.Default in
    let tys =
      List.map
        (function
          | Sqlgg.Sql.Attr attr ->
            ty ~codec:Gen_caml.codec_get_column ~meta:attr.meta attr.domain
          | Dynamic _ ->
            fail ~statement "[%%sql.Request] does not support dynamic columns")
        query.stmt.schema
    in
    let names = List.mapi (fun index _ -> Printf.sprintf "x%d" (index + 1)) tys in
    let build =
      List.fold_right
        (fun name body -> [%expr fun [%p pvar ~loc name] -> [%e body]])
        names
        (match names with
         | [] -> [%expr ()]
         | [ name ] -> evar ~loc name
         | _ -> pexp_tuple ~loc (List.map (evar ~loc) names))
    in
    List.fold_left
      (fun rest col -> [%expr Sqlgg_request.Column ([%e rest], [%e col])])
      [%expr Sqlgg_request.Build [%e build]]
      tys
  in
  let request =
    [%expr
      ({ Sqlgg_request.sql = [%e sql];
         name = [%e Ast_builder.Default.estring ~loc query.name];
         kind = [%e kind];
         params = [%e params];
         result = [%e result] }
        : (_, [%t cardinality]) Sqlgg_request.t)]
  in
  List.fold_right
    (fun name body ->
      Ast_builder.Default.pexp_fun ~loc (Labelled name) None
        (Ast_builder.Default.pvar ~loc name) body)
    (Gen.names_of_vars query.stmt.vars) request
