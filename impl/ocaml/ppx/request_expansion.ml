open Ppxlib

let relocate ~loc =
  object
    inherit Ast_traverse.map
    method! location _ = loc
  end

let expand ~loc ~statement (query : Query.named) =
  let fail fmt = Location.raise_errorf ~loc:statement ("sqlgg: " ^^ fmt) in
  let parsed text = (relocate ~loc)#expression (Parse.expression (Lexing.from_string text)) in
  let scalar ~union (typ : Sqlgg.Sql.Type.t) =
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
    | Union { ctors; _ } -> union ctors
    | Any -> [%expr Sqlgg_request.Any]
  in
  let native =
    scalar ~union:(fun ctors ->
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
                     [ case ~lhs:(pvar ~loc "s") ~guard:None
                         ~rhs:[%expr failwith (Printf.sprintf "Invalid enum value: %s" s)] ])]

            let proj =
              [%e
                pexp_function_cases ~loc
                  (List.map
                     (fun (value, name) ->
                       case ~lhs:(ppat_variant ~loc name None) ~guard:None
                         ~rhs:(estring ~loc value))
                     ctors)]
          end : Sqlgg_traits.Enum with type t = [%t variant])])
  in
  let canonical = scalar ~union:(fun _ -> [%expr Sqlgg_request.Text]) in
  let write ~meta typ writer value =
    Pprintast.string_of_expression
      (match Gen_caml.codec_set_param meta with
       | None -> [%expr [%e writer] Sqlgg_request.Native [%e native typ] [%e parsed value]]
       | Some encode ->
         [%expr
           [%e writer] Sqlgg_request.Canonical [%e canonical typ]
             ([%e parsed encode] [%e parsed value])])
  in
  let sql =
    match Gen.get_sql query with
    | [ `Text text ] ->
      [%expr fun _ -> Sqlgg_request.Sql [%e Ast_builder.Default.estring ~loc text]]
    | pieces ->
      let to_literal meta typ =
        Printf.sprintf "(fun v -> %s)"
          (write ~meta typ [%expr _sqlgg__literals.Sqlgg_request.literal] "v")
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
    | Update table ->
      [%expr
        Sqlgg_query.Update
          [%e
            match table with
            | None -> [%expr None]
            | Some table -> [%expr Some [%e table_name table]]]],
      [%type: Sqlgg_request.affected_rows]
    | Delete tables ->
      [%expr
        Sqlgg_query.Delete
          [%e Ast_builder.Default.elist ~loc (List.map table_name tables)]],
      [%type: Sqlgg_request.affected_rows]
    | Create _ | CreateIndex _ | Alter _ | Drop _ | CreateRoutine _
    | CreateType _ | DropType _ | Other ->
      fail "[%%sql.Request] accepts SELECT, INSERT, UPDATE or DELETE"
  in
  let bound = Gen_caml.exclude_in_vars query.stmt.vars in
  let has_params = Gen_caml.has_set_params bound in
  let params =
    if not has_params then [%expr fun _ -> ()]
    else
      let bind ~meta index (param : Sqlgg.Sql.Type.t Sqlgg.Sql.param) =
        let name = Gen.show_param_name param index in
        let set = write ~meta param.typ [%expr sqlgg__binder.Sqlgg_request.bind] in
        if Gen.is_param_nullable param then
          Gen_caml.set_param_nullable ~null:"sqlgg__binder.Sqlgg_request.null ()"
            ~scrutinee:name ~bound:"v" ~body:(set "v")
        else Gen.output "%s;" (set name)
      in
      [%expr
        fun sqlgg__binder ->
          [%e
            parsed
              (Gen.capture (fun () -> List.iteri (Gen_caml.set_var ~leaf:bind) bound))]]
  in
  let result =
    let open Ast_builder.Default in
    let columns =
      List.mapi
        (fun index column ->
          let name = Printf.sprintf "x%d" (index + 1) in
          match column with
          | Sqlgg.Sql.Attr attr ->
            let nullable = Sqlgg.Sql.Type.is_nullable attr.domain in
            let ty representation scalar =
              if nullable then [%expr Sqlgg_request.Optional ([%e representation], [%e scalar])]
              else [%expr Sqlgg_request.Required ([%e representation], [%e scalar])]
            in
            begin match Gen_caml.codec_get_column attr.meta with
            | None -> name, ty [%expr Sqlgg_request.Native] (native attr.domain), evar ~loc name
            | Some decode ->
              ( name,
                ty [%expr Sqlgg_request.Canonical] (canonical attr.domain),
                [%expr
                  [%e parsed (if nullable then decode ^ "_nullable" else decode)]
                    [%e evar ~loc name]] )
            end
          | Dynamic _ ->
            fail "[%%sql.Request] does not support dynamic columns")
        query.stmt.schema
    in
    let build =
      List.fold_right
        (fun (name, _, _) body -> [%expr fun [%p pvar ~loc name] -> [%e body]])
        columns
        (match List.map (fun (_, _, value) -> value) columns with
         | [] -> [%expr ()]
         | [ value ] -> value
         | values -> pexp_tuple ~loc values)
    in
    List.fold_left
      (fun rest (_, ty, _) -> [%expr Sqlgg_request.Column ([%e rest], [%e ty])])
      [%expr Sqlgg_request.Build [%e build]]
      columns
  in
  let request =
    [%expr
      ({ Sqlgg_request.sql = [%e sql];
         name = [%e Ast_builder.Default.estring ~loc query.name];
         kind = [%e kind];
         param_count =
           [%e
             if has_params then parsed (Gen_caml.eval_count_params bound)
             else [%expr 0]];
         params = [%e params];
         result = [%e result] }
        : (_, _, [%t cardinality]) Sqlgg_request.t)]
  in
  List.fold_right
    (fun name body ->
      Ast_builder.Default.pexp_fun ~loc (Labelled name) None
        (Ast_builder.Default.pvar ~loc name) body)
    (Gen.names_of_vars query.stmt.vars) request
