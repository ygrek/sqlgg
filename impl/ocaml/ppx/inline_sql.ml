open Ppxlib

let fail ~loc fmt = Location.raise_errorf ~loc ("sqlgg: " ^^ fmt)

let locate text_start text =
  let index = Sqlgg.Line_index.make text in
  let advance offset =
    let offset = Sqlgg.Line_index.clamp_offset text offset in
    let line = Sqlgg.Line_index.line index offset in
    let pos_bol =
      if line = 0 then text_start.pos_bol
      else
        text_start.pos_cnum
        + Sqlgg.Line_index.offset index ~line ~character:0
    in
    Ast_builder.Default.position
      ~fname:text_start.pos_fname
      ~lnum:(text_start.pos_lnum + line)
      ~bol:pos_bol
      ~cnum:(text_start.pos_cnum + offset)
  in
  fun (first, last) ->
    Ast_builder.Default.location
      ~start:(advance first)
      ~end_:(advance last)
      ~ghost:false

let fail_diagnostic ~at_span ~statement { Sqlgg.Analysis.message; location } =
  let loc =
    match location with
    | Span pos -> at_span pos
    | Properties | Statement -> statement
  in
  fail ~loc "%s" message

let () =
  let schemas = ref [] in
  let dialect = ref None in
  let params = ref `From_dialect in
  Driver.add_arg "-schema"
    (Arg.String (fun file -> schemas := file :: !schemas))
    ~doc:"<file.sql> DDL for [%sql] (repeatable, replayed in the given order; relative to the preprocessed file)";
  Driver.add_arg "-dialect"
    (Arg.Symbol
       ( List.map Sqlgg.Dialect.to_string Sqlgg.Dialect.all,
         fun value -> dialect := Sqlgg.Dialect.of_string value ))
    ~doc:"<dialect> SQL dialect for [%sql], as sqlgg -dialect (default: mysql)";
  Driver.add_arg "-params"
    (Arg.String (fun s ->
       match Gen.params_mode_of_string s with
       | Ok mode -> params := `Explicit mode
       | Error msg -> raise (Arg.Bad msg)))
    ~doc:"<mode> Parameter substitution for [%sql], as sqlgg -params (default: from -dialect)";
  let schema_state =
    let loaded = ref None in
    fun ~loc ~dialect schema_files ->
      match !loaded with
      | Some state -> state
      | None ->
        let state =
          List.fold_left
            (fun context file ->
              let text =
                match In_channel.with_open_bin file In_channel.input_all with
                | text -> text
                | exception Sys_error msg -> fail ~loc "cannot read -schema file: %s" msg
              in
              match Sqlgg.Analysis.context_of_schema_sql ~context ~dialect text with
              | Ok state -> state
              | Error (block, diagnostic, _) ->
                let at_text =
                  locate { Lexing.pos_fname = file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } text
                in
                fail_diagnostic diagnostic
                  ~at_span:(fun pos -> at_text (Sqlgg.Sql.Pos.shift (fst block.pos) pos))
                  ~statement:(at_text block.pos))
            (Sqlgg.Analysis.with_context Sqlgg.Compile.snapshot)
            schema_files
        in
        loaded := Some state;
        state
  in
  let expand ~ctxt ~arg payload =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let input_name = Expansion_context.Extension.input_name ctxt in
    let schema_files =
      let directory = Filename.dirname input_name in
      List.rev_map
        (fun file ->
          if Filename.is_relative file && directory <> "." then
            Filename.concat directory file
          else
            file)
        !schemas
    in
    let text, literal_loc, at_text =
      Ast_pattern.parse
        Ast_pattern.(single_expr_payload (pexp_constant (pconst_string __ __ __)))
        loc payload
        ~on_error:(fun () ->
          fail ~loc "[%%sql] takes exactly one string literal, e.g. [%%sql {| SELECT ... |}]")
        (fun text literal_loc delimiter ->
          let source_length = literal_loc.loc_end.pos_cnum - literal_loc.loc_start.pos_cnum in
          let at_text =
            match delimiter with
            | None when source_length <> String.length text -> fun _ -> literal_loc
            | Some _ | None -> locate literal_loc.loc_start text
          in
          (text, literal_loc, at_text))
    in
    let selected_dialect =
      Stdlib.Option.value !dialect ~default:Sqlgg.Dialect.MySQL
    in
    Sqlgg.Dialect.set_selected selected_dialect;
    Gen.params_mode := Gen.resolve_params_mode ~dialect:!dialect !params;
    let context = schema_state ~loc ~dialect:selected_dialect schema_files in
    let block =
      match Sqlgg.Statements.split text with
      | [ block ] -> block
      | [] -> fail ~loc:literal_loc "[%%sql] needs an SQL statement"
      | _ :: extra :: _ ->
        fail ~loc:(at_text extra.pos) "[%%sql] takes exactly one statement, this is another one"
    in
    let () =
      match block.errors with
      | [] -> ()
      | (pos, msg) :: _ -> fail ~loc:(at_text pos) "%s" msg
    in
    let reject_property property why =
      fail ~loc:literal_loc
        "[%%sql] does not support the %s property: %s"
        property why
    in
    let reject_migration_property property =
      reject_property property
        "migration properties belong in a sqlgg .sql file"
    in
    List.iter
      (fun (prop : Sqlgg.Props.t) ->
        match prop with
        | Subst _ ->
          reject_property "subst" "it splices runtime strings into the SQL"
        | Noparse ->
          reject_property "noparse" "the statement must be type-checked"
        | Include _ ->
          reject_property "include" "include modes belong in a sqlgg .sql file"
        | Dynamic_select _ ->
          reject_property "dynamic_select"
            "dynamic select modes belong in a sqlgg .sql file"
        | Id _ -> reject_migration_property "id"
        | Down _ -> reject_migration_property "down"
        | Irreversible -> reject_migration_property "irreversible"
        | Auto -> reject_migration_property "generated"
        | Manual -> reject_migration_property "manual"
        | File _ | Noop ->
          reject_property "internal" "this property is reserved for generated queries"
        | Name _ -> ())
      block.props;
    let statement = at_text block.pos in
    let syntax =
      let at_span (first, last) =
        at_text
          (Sqlgg.Sql.Pos.shift (fst block.pos)
             ( Sqlgg.Statements.unstripped_offset block first,
               Sqlgg.Statements.unstripped_offset block last ))
      in
      match Sqlgg.Analysis.analyze ~context (Sqlgg.Statements.strip_comments block) with
      | Error (diagnostic, _) -> fail_diagnostic ~at_span ~statement diagnostic
      | Ok (_, syntax) ->
        match
          Sqlgg.Analysis.dialect_diagnostics
            ~dialect:selected_dialect syntax.dialect_features
        with
        | diagnostic :: _ -> fail_diagnostic ~at_span ~statement diagnostic
        | [] -> syntax
    in
    let () =
      match Sqlgg.Stmt.category_of_stmt_kind syntax.typed.kind with
      | DQL | DML -> ()
      | DDL | DCL | TCL | OTHER ->
        fail ~loc:statement
          "[%%sql] accepts SELECT, INSERT, UPDATE or DELETE; put DDL in a -schema file"
    in
    Stdlib.Option.iter
      (fail ~loc:statement "%s")
      (Gen.params_mode_error ~params_mode:!Gen.params_mode syntax.typed.kind);
    let query =
      Query.of_result
        (Sqlgg.Props.File (Filename.basename input_name) :: block.props)
        syntax
      |> Query.named 0
    in
    let loc = { loc with loc_ghost = true } in
    match arg with
    | Some { txt = Longident.Lident "Request"; _ } ->
      Request_expansion.expand ~loc ~statement query
    | Some _ | None ->
      let code =
        Gen.capture (fun () ->
          Gen_caml.generate_direct_body ~io:`Traits_module [ query ])
      in
      let generated_items =
        (object
          inherit Ast_traverse.map
          method! location _ = loc
        end)#structure
          (Parse.implementation (Lexing.from_string code))
      in
      let traits_path =
        Stdlib.Option.value arg
          ~default:(Ast_builder.Default.Located.lident ~loc "T")
      in
      let traits_module_type =
        Ast_builder.Default.pmty_ident ~loc
          (Ast_builder.Default.Located.lident ~loc "Sqlgg_traits.M_io")
      in
      let generated_functor =
        Ast_builder.Default.pmod_functor ~loc
          (Named
             ( Ast_builder.Default.Located.mk ~loc (Some "T"),
               traits_module_type ))
          (Ast_builder.Default.pmod_structure ~loc generated_items)
      in
      let generated_module =
        Ast_builder.Default.pmod_apply ~loc generated_functor
          (Ast_builder.Default.pmod_ident
             ~loc:traits_path.loc traits_path)
      in
      [%expr
        fun sqlgg__db ->
          let module Sqlgg_inline = [%m generated_module] in
          [%e
            Ast_builder.Default.evar ~loc
              ("Sqlgg_inline." ^ String.uncapitalize_ascii query.name)]
            sqlgg__db]
  in
  Driver.register_transformation "sqlgg.sql"
    ~rules:
      [ Context_free.Rule.extension
          (Extension.V3.declare_with_path_arg
             "sql" Extension.Context.expression Ast_pattern.__ expand)
      ]
