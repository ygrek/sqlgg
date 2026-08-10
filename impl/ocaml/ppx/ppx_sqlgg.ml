open Ppxlib
module D = Ast_builder.Default

let error ~loc fmt =
  Printf.ksprintf (fun msg -> Location.Error.createf ~loc "deriving sqlgg: %s" msg) fmt

type reader =
  | Read_direct
  | Read_nullable
  | Read_nullable_exn

let nullable_row = function
  | Read_direct -> false
  | Read_nullable | Read_nullable_exn -> true

type suffix =
  | Cols of { is_nullable : bool }
  | Read of reader

let string_of_suffix = function
  | Cols { is_nullable = false } -> "cols"
  | Cols { is_nullable = true } -> "nullable_cols"
  | Read Read_direct -> "of_cols"
  | Read Read_nullable -> "of_nullable_cols"
  | Read Read_nullable_exn -> "of_nullable_cols_exn"

let derived_name suffix = Expansion_helpers.mangle (Suffix (string_of_suffix suffix))

let derived_lid suffix lid =
  Loc.map lid ~f:(Expansion_helpers.mangle_lid (Suffix (string_of_suffix suffix)))

let cols_arg = "sqlgg__cols"
let val_arg = Printf.sprintf "sqlgg__v_%s"
let conv_arg = Printf.sprintf "sqlgg__conv_%s"

let ctx = Attribute.Context.label_declaration

let located name pat =
  Attribute.declare_with_attr_loc name ctx pat (fun ~attr_loc x ->
      Loc.make ~loc:attr_loc x)

let col_attr = located "sqlgg.col" Ast_pattern.(single_expr_payload (estring __))

let map_attr =
  located "sqlgg.map"
    Ast_pattern.(
      map1 (single_expr_payload __) ~f:(fun e -> Either.Left e)
      ||| map1 (ptyp __) ~f:(fun t -> Either.Right t))
let default_attr = located "sqlgg.default" Ast_pattern.(single_expr_payload __)
let by_attr = Attribute.declare_flag "sqlgg.by" ctx

let nested_attr =
  Attribute.declare "sqlgg.nested" ctx
    Ast_pattern.(
      map0 (pstr nil) ~f:false
      ||| map0
            (single_expr_payload (pexp_ident (lident (string "default_none"))))
            ~f:true)
    (fun x -> x)

let attributes =
  [ Attribute.T col_attr
  ; Attribute.T map_attr
  ; Attribute.T default_attr
  ; Attribute.T by_attr
  ; Attribute.T nested_attr
  ]

type 'a how =
  | Plain
  | Defaulted of expression
  | Mapped of 'a

type 'a conv =
  { by : bool
  ; is_nullable : bool
  ; how : 'a how
  }

type nesting =
  | Nested
  | Joined_opt
  | Joined_default_none

type 'a column =
  { col : string loc
  ; conv : 'a conv
  ; fty : core_type
  }

type sub_ref =
  { nesting : nesting
  ; lid : longident loc
  }

type 'a source =
  | Column of 'a column
  | Sub of sub_ref

type 'src fld =
  { fname : string
  ; floc : location
  ; src : 'src
  }

let columns fields =
  List.filter_map
    (fun f -> match f.src with Column c -> Some { f with src = c } | Sub _ -> None)
    fields

let subs fields =
  List.filter_map
    (fun f -> match f.src with Sub s -> Some { f with src = s } | Column _ -> None)
    fields

let col_ty ~loc t =
  [%type: ([%t t], 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col]

let cols_var ~loc = [%type: 'sqlgg__cols]

let cols_params ~loc =
  [ [%type: 'sqlgg__brand]
  ; [%type: 'sqlgg__row]
  ; [%type: 'sqlgg__params]
  ; cols_var ~loc
  ]
let record_ty ~loc tname = D.ptyp_constr ~loc (D.Located.lident ~loc tname) []
let opaque f = D.ptyp_var ~loc:f.floc (Printf.sprintf "sqlgg__raw_%s" f.fname)
let fill_default ~loc e = [%expr Stdlib.Option.value ~default:[%e e]]

let match_ pat (ty : core_type) =
  Ast_pattern.parse_res pat ty.ptyp_loc ty (fun r -> r) |> Result.to_option

let option_arg =
  match_
    Ast_pattern.(
      ptyp_constr
        (lident (string "option") ||| ldot (lident (string "Stdlib")) (string "option"))
        (__ ^:: nil))

type presence =
  | Witnesses
  | Votes
  | Silent

let conv_presence { by; is_nullable; how } =
  match how, is_nullable, by with
  | Defaulted _, _, _ -> Votes
  | (Plain | Mapped _), false, _ -> Witnesses
  | Plain, true, false -> Votes
  | Plain, true, true -> Silent
  | Mapped _, true, _ -> Silent

let presence f =
  match f.src with
  | Sub { nesting = Nested; _ } -> Witnesses
  | Sub { nesting = Joined_opt | Joined_default_none; _ } -> Votes
  | Column c -> conv_presence c.conv

type 'a raw_kind =
  | As_is
  | Optional
  | Opaque of 'a option

let raw_kind { by; how; _ } =
  match how, by with
  | Plain, false -> As_is
  | Defaulted _, _ -> Optional
  | Plain, true -> Opaque None
  | Mapped conv, _ -> Opaque (Some conv)

let raw_ty ~loc ~converted f c =
  match raw_kind c with
  | As_is -> f.src.fty
  | Optional -> [%type: [%t f.src.fty] option]
  | Opaque x -> converted f x

let impl_raw ~loc = raw_ty ~loc ~converted:(fun f _ -> opaque f)
let sig_raw ~loc = raw_ty ~loc ~converted:(fun f t -> Option.value t ~default:(opaque f))

let cols_obj ~loc ~is_nullable ~raw cols =
  D.ptyp_object ~loc
    (List.map
       (fun c ->
         let loc = c.src.col.loc in
         let t = raw ~loc c c.src.conv in
         let t =
           match is_nullable, conv_presence c.src.conv with
           | false, _ | true, (Votes | Silent) -> t
           | true, Witnesses -> [%type: [%t t] option]
         in
         D.otag ~loc c.src.col (col_ty ~loc t))
       cols)
    Open

let by_cols cols = List.filter (fun c -> c.src.conv.by) cols

let by_label c =
  match c.src.conv.how with
  | Plain -> Labelled c.fname
  | Mapped _ | Defaulted _ -> Optional c.fname

let by_default ~loc = function
  | Plain -> None
  | Mapped conv -> Some conv
  | Defaulted e -> Some (fill_default ~loc e)

let child_reader rd nesting =
  match rd, nesting with
  | Read_direct, Nested -> Read_direct
  | Read_nullable, _ -> Read_nullable
  | _, Joined_opt -> Read_nullable_exn
  | _, (Nested | Joined_default_none) -> Read_nullable

type 'a witness =
  { strict : 'a source fld list
  ; extra : 'a source fld list
  }

let witness fields =
  let strict, extra =
    List.fold_right
      (fun f (strict, extra) ->
        match presence f with
        | Witnesses -> f :: strict, extra
        | Votes -> strict, f :: extra
        | Silent -> strict, extra)
      fields ([], [])
  in
  match strict with
  | [] -> None
  | _ -> Some { strict; extra }

let resolve_field ~pick_spec ld =
  let ferr ~loc fmt =
    Printf.ksprintf
      (fun m -> Error (error ~loc "field %s: %s" ld.pld_name.txt m))
      fmt
  in
  let col = Attribute.get col_attr ld in
  let map = Attribute.get map_attr ld in
  let default = Attribute.get default_attr ld in
  let by = Attribute.has_flag by_attr ld in
  let opt_ty = option_arg ld.pld_type in
  let is_nullable = Option.is_some opt_ty in
  let column how =
    Column
      { col = Option.value col ~default:ld.pld_name
      ; conv = { by; is_nullable; how }
      ; fty = ld.pld_type
      }
  in
  let nested nesting ty =
    let clash =
      List.filter_map
        (fun (name, present) -> if present then Some name else None)
        [ "[@sqlgg.col]", Option.is_some col
        ; "[@sqlgg.map]", Option.is_some map
        ; "[@sqlgg.default]", Option.is_some default
        ; "[@sqlgg.by]", by
        ]
    in
    match clash, match_ Ast_pattern.(ptyp_constr __' nil) ty with
    | (_ :: _ as names), _ ->
      ferr ~loc:ld.pld_loc "[@sqlgg.nested] cannot be combined with %s"
        (String.concat ", " names)
    | [], Some lid -> Ok (Sub { nesting; lid })
    | [], None ->
      ferr ~loc:ld.pld_loc "[@sqlgg.nested] needs a record type deriving sqlgg"
  in
  let source =
    let default_none = Attribute.get nested_attr ld in
    match default_none, opt_ty with
    | Some false, None -> nested Nested ld.pld_type
    | Some false, Some inner -> nested Joined_opt inner
    | Some true, Some inner -> nested Joined_default_none inner
    | Some true, None ->
      ferr ~loc:ld.pld_loc "[@sqlgg.nested default_none] needs an option field"
    | None, _ ->
      match map, default with
      | Some _, Some { loc; _ } ->
        ferr ~loc "[@sqlgg.map] and [@sqlgg.default] cannot be combined"
      | Some m, None ->
        pick_spec m.txt
        |> Result.map_error (fun why ->
               error ~loc:m.loc "field %s: [@sqlgg.map]: %s" ld.pld_name.txt why)
        |> Result.map (fun conv -> column (Mapped conv))
      | None, Some { txt; loc } ->
        if is_nullable then
          ferr ~loc "[@sqlgg.default] replaces NULL, so the field cannot be an option"
        else Ok (column (Defaulted txt))
      | None, None -> Ok (column Plain)
  in
  Result.map
    (fun src -> { fname = ld.pld_name.txt; floc = ld.pld_loc; src })
    source

let cols_decls ~loc ~nullable tname subs cols =
  let open (val Ast_builder.make loc) in
  let cols_ty = cols_var ~loc in
  let vars = cols_params ~loc in
  let decl rd =
    type_declaration
      ~name:(Loc.make ~loc (derived_name (Cols { is_nullable = nullable_row rd }) tname))
      ~params:(List.map (fun v -> v, (NoVariance, NoInjectivity)) vars)
      ~cstrs:
        ((cols_ty, cols_obj ~loc ~is_nullable:(nullable_row rd) ~raw:impl_raw cols, loc)
        :: List.map
             (fun { src = { nesting; lid }; _ } ->
               ( cols_ty
               , ptyp_constr
                   (derived_lid
                      (Cols { is_nullable = nullable_row (child_reader rd nesting) })
                      lid)
                   vars
               , loc ))
             subs)
      ~kind:Ptype_abstract ~private_:Public ~manifest:(Some cols_ty)
  in
  let nameable c =
    match c.src.conv.by, c.src.conv.how with true, Plain -> false | _ -> true
  in
  match List.for_all nameable cols, nullable with
  | false, _ -> []
  | true, false -> [ decl Read_direct ]
  | true, true -> [ decl Read_direct; decl Read_nullable_exn ]

type 'a resolved =
  { loc : location
  ; tname : string
  ; first : 'a source fld
  ; rest : 'a source fld list
  ; fields : 'a source fld list
  ; cols : 'a column fld list
  ; subs : sub_ref fld list
  ; witness : 'a witness option
  ; decls : type_declaration list
  }

let build { loc; tname; first; rest; fields; cols; witness; decls; _ } =
  let open (val Ast_builder.make loc) in
  let record_ty = record_ty ~loc tname in
  let item suffix body = [%stri let [%p pvar (derived_name suffix tname)] = [%e body]] in
  let scoped ~exp ~body =
    let bind op f =
      D.binding_op ~loc:f.floc
        ~op:(Loc.make ~loc:f.floc op)
        ~pat:(D.pvar ~loc:f.floc (val_arg f.fname))
        ~exp:(exp f)
    in
    let letop =
      pexp_letop
        (letop ~let_:(bind "let+" first) ~ands:(List.map (bind "and+") rest) ~body)
    in
    [%expr
      let open Sqlgg_scope in
      [%e letop]]
  in
  let record ~value =
    [%expr
      ([%e
         pexp_record
           (List.map (fun f -> D.Located.lident ~loc:f.floc f.fname, value f) fields)
           None]
        : [%t record_ty])]
  in
  let readers =
    let read rd f =
      match f.src with
      | Sub { nesting; lid } ->
        let loc = f.floc in
        [%expr
          [%e
            D.pexp_ident ~loc
              (derived_lid (Read (child_reader rd nesting)) lid)]
            [%e D.evar ~loc cols_arg]]
      | Column { col; _ } -> D.pexp_send ~loc:col.loc (D.evar ~loc:col.loc cols_arg) col
    in
    let value f =
      let v = D.evar ~loc:f.floc (val_arg f.fname) in
      match f.src with
      | Sub _ -> v
      | Column { col = { loc; _ }; conv; _ } ->
        match conv with
        | { by = false; how = Plain; _ } -> v
        | { by = false; how = Defaulted e; _ } -> [%expr [%e fill_default ~loc e] [%e v]]
        | { by = false; how = Mapped conv; _ } -> [%expr [%e conv] [%e v]]
        | { by = true; _ } -> [%expr [%e D.evar ~loc (conv_arg f.fname)] [%e v]]
    in
    let bare = record ~value in
    let reader_item rd ~result body =
      item (Read rd)
        (List.fold_right
           (fun (lbl, default, p) acc -> D.pexp_fun ~loc:p.ppat_loc lbl default p acc)
           (List.map
              (fun c ->
                ( by_label c
                , by_default ~loc:c.floc c.src.conv.how
                , D.pvar ~loc:c.floc (conv_arg c.fname) ))
              (by_cols cols)
           @ [ ( Nolabel
               , None
               , [%pat?
                   ([%p pvar cols_arg]
                     : [%t
                         cols_obj ~loc ~is_nullable:(nullable_row rd) ~raw:impl_raw cols])]
               )
             ])
           [%expr ([%e scoped ~exp:(read rd) ~body] : [%t col_ty ~loc result])])
    in
    let tuple_pat ~strict:pat ~rest strict extra =
      ppat_tuple (List.map pat strict @ List.map rest extra)
    in
    let none f = let loc = f.floc in [%pat? None] in
    let present =
      tuple_pat
        ~strict:(fun f ->
          let loc = f.floc in
          [%pat? Some [%p D.pvar ~loc (val_arg f.fname)]])
        ~rest:(fun f -> D.ppat_any ~loc:f.floc)
    in
    let arm lhs rhs = case ~lhs ~guard:None ~rhs in
    let missing strict extra =
      List.mapi
        (fun i f ->
          let loc = f.floc in
          arm
            (ppat_tuple
               (List.mapi
                  (fun j g -> if i = j then [%pat? None] else D.ppat_any ~loc:g.floc)
                  strict
               @ List.map (fun g -> D.ppat_any ~loc:g.floc) extra))
            [%expr
              failwith
                [%e
                  D.estring ~loc (Printf.sprintf "sqlgg: %s.%s is NULL" tname f.fname)]])
        strict
    in
    let opt_result = [%type: [%t record_ty] option] in
    let witnessed =
      match witness with
      | None -> []
      | Some { strict; extra } ->
        let opt_arms =
          [ arm (present strict extra) [%expr Some [%e bare]]
          ; arm (tuple_pat ~strict:none ~rest:none strict extra) [%expr None]
          ]
          @
          match strict, extra with
          | [ _ ], [] -> []
          | _ -> missing strict extra
        in
        [ reader_item Read_nullable ~result:opt_result
            (List.fold_right
               (fun f acc ->
                 let loc = f.floc in
                 [%expr
                   Stdlib.Option.bind [%e D.evar ~loc (val_arg f.fname)] (fun
                       [%p D.pvar ~loc (val_arg f.fname)] -> [%e acc])])
               strict
               [%expr Some [%e bare]])
        ; reader_item Read_nullable_exn ~result:opt_result
            (pexp_match
               (pexp_tuple
                  (List.map (fun f -> evar (val_arg f.fname)) (strict @ extra)))
               opt_arms)
        ]
    in
    reader_item Read_direct ~result:record_ty bare :: witnessed
  in
  List.map (fun d -> pstr_type Recursive [ d ]) decls @ readers

let sig_body { loc; tname; cols; subs; witness; decls; _ } =
  let open (val Ast_builder.make loc) in
  let record_ty = record_ty ~loc tname in
  let value suffix type_ =
    psig_value
      (value_description
         ~name:(Loc.make ~loc (derived_name suffix tname))
         ~type_ ~prim:[])
  in
  let reader ~is_nullable result =
    List.fold_right
      (fun c acc ->
        ptyp_arrow (by_label c)
          [%type: [%t sig_raw ~loc:c.floc c c.src.conv] -> [%t c.src.fty]]
          acc)
      (by_cols cols)
      [%type:
        [%t
          match subs with
          | [] -> cols_obj ~loc ~is_nullable ~raw:sig_raw cols
          | _ :: _ ->
            ptyp_constr
              (D.Located.lident ~loc (derived_name (Cols { is_nullable }) tname))
              (cols_params ~loc)]
        -> [%t col_ty ~loc result]]
  in
  let opt_result = [%type: [%t record_ty] option] in
  let witnessed =
    match witness with
    | None -> []
    | Some _ ->
      [ value (Read Read_nullable) (reader ~is_nullable:true opt_result)
      ; value (Read Read_nullable_exn) (reader ~is_nullable:true opt_result)
      ]
  in
  List.map (fun d -> psig_type Recursive [ d ]) decls
  @ value (Read Read_direct) (reader ~is_nullable:false record_ty) :: witnessed

let sig_items ({ loc; tname; subs; decls; _ } as a) =
  let open (val Ast_builder.make loc) in
  match subs, decls with
  | (_ :: _ as ss), [] ->
    List.map
      (fun f ->
        psig_extension
          (Location.Error.to_extension
             (error ~loc:f.floc
                "[@sqlgg.nested] needs %s, and the parent cannot pass the \
                 [@sqlgg.by] argument"
                (derived_name (Cols { is_nullable = false }) tname)))
          [])
      ss
  | _, _ -> sig_body a

let dispatch ~loc ~ext ~pick_spec ~record ~nullable tds =
  let emit errs =
    List.map
      (fun e -> ext ~loc:(Location.Error.get_location e) (Location.Error.to_extension e))
      errs
  in
  List.concat_map
    (fun td ->
      match td.ptype_params, td.ptype_kind with
      | _ :: _, _ -> emit [ error ~loc:td.ptype_loc "type parameters are not supported" ]
      | [], Ptype_record [] -> emit [ error ~loc:td.ptype_loc "record has no fields" ]
      | [], (Ptype_abstract | Ptype_variant _ | Ptype_open) ->
        emit [ error ~loc:td.ptype_loc "only record types are supported" ]
      | [], Ptype_record (ld :: lds) ->
        let step ld (fs, es) =
          match resolve_field ~pick_spec ld with
          | Ok f -> f :: fs, es
          | Error e -> fs, e :: es
        in
        match resolve_field ~pick_spec ld, List.fold_right step lds ([], []) with
        | Error e, (_, es) -> emit (e :: es)
        | Ok _, (_, (_ :: _ as es)) -> emit es
        | Ok f, (fs, []) ->
        let fields = f :: fs in
        let witness =
          match nullable, witness fields with
          | false, _ -> Ok None
          | true, (Some _ as w) -> Ok w
          | true, None ->
            Error
              (error ~loc:td.ptype_loc
                 "~nullable_cols needs a column that cannot be NULL, or drop the option")
        in
        match witness with
        | Error e -> emit [ e ]
        | Ok witness ->
          let cols = columns fields in
          let subs = subs fields in
          record
            { loc
            ; tname = td.ptype_name.txt
            ; first = f
            ; rest = fs
            ; fields
            ; cols
            ; subs
            ; witness
            ; decls =
                cols_decls ~loc ~nullable:(Option.is_some witness) td.ptype_name.txt
                  subs cols
            })
    tds

let expand ~ctxt (_rec_flag, tds) nullable =
  dispatch
    ~loc:(Expansion_context.Deriver.derived_item_loc ctxt)
    ~ext:(fun ~loc e -> D.pstr_extension ~loc e [])
    ~pick_spec:
      (function
      | Either.Left e -> Ok e
      | Either.Right _ -> Error "expected a conversion function")
    ~record:build ~nullable tds

let expand_sig ~ctxt (_rec_flag, tds) nullable =
  dispatch
    ~loc:(Expansion_context.Deriver.derived_item_loc ctxt)
    ~ext:(fun ~loc e -> D.psig_extension ~loc e [])
    ~pick_spec:
      (function
      | Either.Right t -> Ok t
      | Either.Left _ -> Error "expected a column type after a colon")
    ~record:sig_items ~nullable tds

let () =
  Deriving.add "sqlgg"
    ~str_type_decl:
      (Deriving.Generator.V2.make ~attributes
         Deriving.Args.(empty +> flag "nullable_cols")
         expand)
    ~sig_type_decl:
      (Deriving.Generator.V2.make ~attributes
         Deriving.Args.(empty +> flag "nullable_cols")
         expand_sig)
  |> Deriving.ignore
