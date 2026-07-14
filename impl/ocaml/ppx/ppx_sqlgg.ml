open Ppxlib

let fun_name = function "t" -> "of_cols" | t -> t ^ "_of_cols"

let build ~loc tname (fields : label_declaration list) =
  let (module B) = Ast_builder.make loc in
  let open B in
  let lid = Located.lident in
  let record_ty = ptyp_constr (lid tname) [] in
  let names = List.map (fun ld -> ld.pld_name.txt) fields in
  let ctor =
    eabstract (List.map pvar names)
      [%expr
        ([%e pexp_record (List.map (fun n -> (lid n, evar n)) names) None]
          : [%t record_ty])]
  in
  let col_ty field_ty =
    [%type:
      ([%t field_ty], 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col]
  in
  let cols_ty =
    ptyp_object
      (List.map (fun ld -> otag ld.pld_name (col_ty ld.pld_type)) fields)
      Open
  in
  let body =
    List.fold_left
      (fun acc n ->
        [%expr Sqlgg_scope.apply [%e acc] [%e pexp_send [%expr sqlgg__cols] (Located.mk n)]])
      [%expr Sqlgg_scope.pure [%e ctor]]
      names
  in
  [ [%stri
      let [%p pvar (fun_name tname)] =
       fun (sqlgg__cols : [%t cols_ty]) : [%t col_ty record_ty] -> [%e body]]
  ]

let () =
  Deriving.add "sqlgg"
    ~str_type_decl:
      (Deriving.Generator.V2.make Deriving.Args.empty (fun ~ctxt (_, tds) ->
           let loc = Expansion_context.Deriver.derived_item_loc ctxt in
           List.concat_map
             (fun td ->
               Ast_pattern.(parse (ptype_record __)) loc td.ptype_kind
                 ~on_error:(fun () ->
                   [ [%stri
                       [%%ocaml.error
                       "deriving sqlgg: only record types are supported"]] ])
                 (build ~loc td.ptype_name.txt))
             tds))
  |> Deriving.ignore
