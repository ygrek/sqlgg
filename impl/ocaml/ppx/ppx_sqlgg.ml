open Ppxlib
module D = Ast_builder.Default

let derived_name ~suffix = Expansion_helpers.mangle (Suffix suffix)

let derived_lid ~suffix lid =
  Loc.map lid ~f:(Expansion_helpers.mangle_lid (Suffix suffix))

let cols_arg = "sqlgg__cols"
let fn_arg = "sqlgg__f"
let rec_arg = "sqlgg__r"
let col_arg = Printf.sprintf "sqlgg__c_%s"
let val_arg = Printf.sprintf "sqlgg__v_%s"
let conv_arg = Printf.sprintf "sqlgg__conv_%s"

type spec =
  | Fn of expression
  | Raw of core_type

let ctx = Attribute.Context.label_declaration

let located name pat =
  Attribute.declare_with_attr_loc name ctx pat (fun ~attr_loc x ->
      Loc.make ~loc:attr_loc x)

let spec_attr name =
  located name
    Ast_pattern.(
      map1 (single_expr_payload __) ~f:(fun e -> Fn e)
      ||| map1 (ptyp __) ~f:(fun t -> Raw t))

let col_attr = located "sqlgg.col" Ast_pattern.(single_expr_payload (estring __))
let map_attr = spec_attr "sqlgg.map"
let set_attr = spec_attr "sqlgg.set"
let default_attr = located "sqlgg.default" Ast_pattern.(single_expr_payload __)
let by_attr = Attribute.declare_flag "sqlgg.by" ctx
let nested_attr = Attribute.declare_flag "sqlgg.nested" ctx

let attributes =
  [ Attribute.T col_attr; Attribute.T map_attr; Attribute.T set_attr
  ; Attribute.T default_attr; Attribute.T by_attr; Attribute.T nested_attr ]

type 'a how =
  | Plain
  | Defaulted of expression
  | Mapped of 'a

type 'a conv = { by : bool; how : 'a how }

type 'a source =
  | Column of { col : string loc; conv : 'a conv; set : 'a option }
  | Group of longident loc

type 'a field =
  { fname : string
  ; floc : location
  ; fty : core_type
  ; src : 'a source
  }

type binder =
  | Anon
  | Named of string
  | Named_opt of string * expression

type param =
  { pkind : binder
  ; ppat : pattern
  ; ploc : location
  }

let efun params body =
  List.fold_right
    (fun { pkind; ppat; ploc = loc } acc ->
      let lbl, def =
        match pkind with
        | Anon -> Nolabel, None
        | Named n -> Labelled n, None
        | Named_opt (n, d) -> Optional n, Some d
      in
      D.pexp_fun ~loc lbl def ppat acc)
    params body

let col_ty ~loc t =
  [%type: ([%t t], 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col]

let scope_vars ~loc =
  [ [%type: 'sqlgg__brand]; [%type: 'sqlgg__row]; [%type: 'sqlgg__params] ]

let row_var ~loc = [%type: 'sqlgg__cols]

let record_ty ~loc tname = D.ptyp_constr ~loc (Loc.make ~loc (Lident tname)) []
let opaque f = D.ptyp_var ~loc:f.floc (Printf.sprintf "sqlgg__raw_%s" f.fname)

let intrinsic ~loc f = function
  | { by = false; how = Plain } -> Some f.fty
  | { by = false; how = Defaulted _ } -> Some [%type: [%t f.fty] option]
  | { by = true; _ } | { how = Mapped _; _ } -> None
let fill_default ~loc e = [%expr Stdlib.Option.value ~default:[%e e]]

let analyze_field ~err ~pick ld =
  let loc = ld.pld_loc in
  let is_option ty =
    match ty.ptyp_desc with
    | Ptyp_constr ({ txt = Lident "option" | Ldot (_, "option"); _ }, [ _ ]) -> true
    | _ -> false
  in
  let nullary_constr ty =
    match ty.ptyp_desc with Ptyp_constr (lid, []) -> Some lid | _ -> None
  in
  let errf ~loc fmt =
    Printf.ksprintf
      (fun msg -> err (loc, Printf.sprintf "field %s: %s" ld.pld_name.txt msg))
      fmt
  in
  let picked name { txt; loc } =
    match pick txt with
    | Ok x -> Some x
    | Error why ->
      errf ~loc "%s: %s" name why;
      None
  in
  let col = Attribute.get col_attr ld in
  let map = Attribute.get map_attr ld in
  let default = Attribute.get default_attr ld in
  let set_attr_value = Attribute.get set_attr ld in
  let by = Attribute.has_flag by_attr ld in
  let how =
    match map, default with
    | Some _, Some { loc; _ } ->
      errf ~loc "[@sqlgg.map] and [@sqlgg.default] cannot be combined";
      Some Plain
    | Some m, None -> Option.map (fun x -> Mapped x) (picked "[@sqlgg.map]" m)
    | None, Some { txt; loc } ->
      if is_option ld.pld_type then
        errf ~loc "[@sqlgg.default] replaces NULL, so the field cannot be an option";
      Some (Defaulted txt)
    | None, None -> Some Plain
  in
  let set =
    match set_attr_value with
    | None -> Some None
    | Some s -> Option.map Option.some (picked "[@sqlgg.set]" s)
  in
  let used =
    List.filter_map
      (fun (name, present) -> if present then Some name else None)
      [ "[@sqlgg.col]", Option.is_some col
      ; "[@sqlgg.map]", Option.is_some map
      ; "[@sqlgg.default]", Option.is_some default
      ; "[@sqlgg.set]", Option.is_some set_attr_value
      ; "[@sqlgg.by]", by ]
  in
  match how, set with
  | None, _ | _, None -> None
  | Some how, Some set ->
    let column () =
      Column { col = Option.value col ~default:ld.pld_name; conv = { by; how }; set }
    in
    let src =
      match Attribute.has_flag nested_attr ld, used, nullary_constr ld.pld_type with
      | false, _, _ -> column ()
      | true, (_ :: _ as names), _ ->
        errf ~loc "[@sqlgg.nested] cannot be combined with %s"
          (String.concat ", " names);
        column ()
      | true, [], Some lid -> Group lid
      | true, [], None ->
        errf ~loc "[@sqlgg.nested] needs a record type deriving sqlgg";
        column ()
    in
    Some { fname = ld.pld_name.txt; floc = loc; fty = ld.pld_type; src }

let cols_type ~loc tname fields =
  let (module B) = Ast_builder.make loc in
  let row = row_var ~loc in
  let vars = scope_vars ~loc @ [ row ] in
  let meth f conv = Option.map (col_ty ~loc) (intrinsic ~loc f conv) in
  let step f acc =
    Option.bind acc (fun (meths, cstrs) ->
        match f.src with
        | Group lid ->
          Some
            ( meths
            , (row, B.ptyp_constr (derived_lid ~suffix:"cols" lid) vars, loc) :: cstrs )
        | Column { col; conv; _ } ->
          Option.map (fun t -> B.otag col t :: meths, cstrs) (meth f conv))
  in
  List.fold_right step fields (Some ([], []))
  |> Option.map (fun (meths, cstrs) ->
         B.type_declaration
           ~name:(B.Located.mk (derived_name ~suffix:"cols" tname))
           ~params:(List.map (fun v -> v, (NoVariance, NoInjectivity)) vars)
           ~cstrs:((row, B.ptyp_object meths Open, loc) :: cstrs)
           ~kind:Ptype_abstract ~private_:Public ~manifest:(Some row))

let build ~loc tname first rest =
  let fields = first :: rest in
  let (module B) = Ast_builder.make loc in
  let record_ty = record_ty ~loc tname in
  let item ~suffix body =
    [%stri let [%p B.pvar (derived_name ~suffix tname)] = [%e body]]
  in

  let gen_item =
    let bind op f =
      let loc = f.floc in
      let (module B) = Ast_builder.make loc in
      B.binding_op ~op:(B.Located.mk op)
        ~pat:(B.pvar (val_arg f.fname))
        ~exp:(B.evar (col_arg f.fname))
    in
    let record =
      B.pexp_record
        (List.map (fun f -> B.Located.lident f.fname, B.evar (val_arg f.fname)) fields)
        None
    in
    let letop =
      B.pexp_letop
        (B.letop ~let_:(bind "let+" first) ~ands:(List.map (bind "and+") rest)
           ~body:[%expr ([%e record] : [%t record_ty])])
    in
    let params =
      List.map
        (fun f ->
          let loc = f.floc in
          let (module B) = Ast_builder.make loc in
          { pkind = Named f.fname
          ; ppat = [%pat? ([%p B.pvar (col_arg f.fname)] : [%t col_ty ~loc f.fty])]
          ; ploc = loc
          })
        fields
    in
    item ~suffix:"of_cols_gen"
      (efun params
         [%expr
           ((let open Sqlgg_scope in
             [%e letop])
             : [%t col_ty ~loc record_ty])])
  in

  let of_cols_item =
    let column f col ({ by; how } as conv) =
      let loc = col.loc in
      let (module B) = Ast_builder.make loc in
      let get = B.pexp_send (B.evar cols_arg) col in
      let via e = [%expr Sqlgg_scope.map [%e e] [%e get]] in
      let read =
        match by, how with
        | false, Plain -> get
        | false, Defaulted e -> via (fill_default ~loc e)
        | false, Mapped g -> via g
        | true, _ -> via (B.evar (conv_arg f.fname))
      in
      let ty = Option.value (intrinsic ~loc f conv) ~default:(opaque f) in
      Some (B.otag col (col_ty ~loc ty)), read
    in
    let meths, args =
      List.split
        (List.map
           (fun f ->
             let loc = f.floc in
             let (module B) = Ast_builder.make loc in
             let meth, read =
               match f.src with
               | Group lid ->
                 ( None
                 , [%expr
                     [%e B.pexp_ident (derived_lid ~suffix:"of_cols" lid)]
                       [%e B.evar cols_arg]] )
               | Column { col; conv; _ } -> column f col conv
             in
             meth, (Labelled f.fname, read))
           fields)
    in
    let params =
      List.filter_map
        (fun f ->
          let loc = f.floc in
          match f.src with
          | Group _ | Column { conv = { by = false; _ }; _ } -> None
          | Column { conv = { by = true; how }; _ } ->
            let pkind =
              match how with
              | Plain -> Named f.fname
              | Mapped e -> Named_opt (f.fname, e)
              | Defaulted e -> Named_opt (f.fname, fill_default ~loc e)
            in
            Some { pkind; ppat = D.pvar ~loc (conv_arg f.fname); ploc = loc })
        fields
    in
    let row =
      { pkind = Anon
      ; ppat =
          B.ppat_constraint (B.pvar cols_arg)
            (B.ptyp_object (List.filter_map Fun.id meths) Open)
      ; ploc = loc
      }
    in
    let call = B.pexp_apply (B.evar (derived_name ~suffix:"of_cols_gen" tname)) args in
    item ~suffix:"of_cols"
      (efun (params @ [ row ]) [%expr ([%e call] : [%t col_ty ~loc record_ty])])
  in

  let apply_item =
    let field_of ?set f =
      let loc = f.floc in
      let (module B) = Ast_builder.make loc in
      let v = B.pexp_field (B.evar rec_arg) (B.Located.lident f.fname) in
      match set with None -> v | Some g -> [%expr [%e g] [%e v]]
    in
    let flush acc = function
      | [] -> acc
      | pending ->
        B.pexp_apply acc
          (List.rev_map (fun (f, col, set) -> Labelled col.txt, field_of ?set f) pending)
    in
    let rec chain acc pending = function
      | [] -> flush acc pending
      | ({ src = Group lid; _ } as f) :: tl ->
        let loc = f.floc in
        chain
          [%expr
            [%e D.pexp_ident ~loc (derived_lid ~suffix:"apply" lid)]
              [%e flush acc pending] [%e field_of f]]
          [] tl
      | ({ src = Column { col; set; _ }; _ } as f) :: tl ->
        chain acc ((f, col, set) :: pending) tl
    in
    item ~suffix:"apply"
      [%expr
        fun [%p B.pvar fn_arg] ([%p B.pvar rec_arg] : [%t record_ty]) ->
          [%e chain (B.evar fn_arg) [] fields]]
  in

  let cols_item =
    match cols_type ~loc tname fields with
    | None -> []
    | Some decl -> [ B.pstr_type Recursive [ decl ] ]
  in
  cols_item @ [ gen_item; of_cols_item; apply_item ]

let sig_items ~loc tname first rest =
  let fields = first :: rest in
  let (module B) = Ast_builder.make loc in
  let record_ty = record_ty ~loc tname in
  let sraw f = function
    | { how = Mapped t; _ } -> t
    | { by = true; how = Plain } -> opaque f
    | { how = Plain | Defaulted _; _ } as conv ->
      Option.value (intrinsic ~loc f conv) ~default:[%type: [%t f.fty] option]
  in
  let cols_decl = cols_type ~loc tname fields in
  let groups =
    List.filter (fun f -> match f.src with Group _ -> true | Column _ -> false) fields
  in
  match cols_decl, groups with
  | None, (_ :: _ as blocked) ->
    List.map
      (fun f ->
        D.psig_extension ~loc:f.floc
          (Location.error_extensionf ~loc:f.floc
             "deriving sqlgg: [@sqlgg.nested] needs %s, which a converted column \
              rules out"
             (derived_name ~suffix:"cols" tname))
          [])
      blocked
  | _ ->
    let nests = groups <> [] in
    let value name type_ =
      B.psig_value (B.value_description ~name:(B.Located.mk name) ~type_ ~prim:[])
    in
    let gen =
      List.fold_right
        (fun f acc -> B.ptyp_arrow (Labelled f.fname) (col_ty ~loc f.fty) acc)
        fields (col_ty ~loc record_ty)
    in
    let of_cols =
      let row =
        if nests then
          B.ptyp_constr
            (B.Located.lident (derived_name ~suffix:"cols" tname))
            (scope_vars ~loc @ [ row_var ~loc ])
        else
          B.ptyp_object
            (List.filter_map
               (fun f ->
                 match f.src with
                 | Group _ -> None
                 | Column { col; conv; _ } ->
                   Some (B.otag col (col_ty ~loc (sraw f conv))))
               fields)
            Open
      in
      let arg f conv =
        let lbl =
          match conv with
          | { by = false; _ } -> None
          | { by = true; how = Plain } -> Some (Labelled f.fname)
          | { by = true; how = Defaulted _ | Mapped _ } -> Some (Optional f.fname)
        in
        Option.map (fun l -> l, [%type: [%t sraw f conv] -> [%t f.fty]]) lbl
      in
      List.fold_right
        (fun f acc ->
          match f.src with
          | Group _ -> acc
          | Column { conv; _ } ->
            Option.fold (arg f conv) ~none:acc ~some:(fun (l, ty) ->
                B.ptyp_arrow l ty acc))
        fields
        [%type: [%t row] -> [%t col_ty ~loc record_ty]]
    in
    let apply =
      let callback =
        List.fold_right
          (fun f acc ->
            match f.src with
            | Group _ -> acc
            | Column { col; set; _ } ->
              B.ptyp_arrow (Labelled col.txt) (Option.value set ~default:f.fty) acc)
          fields [%type: 'sqlgg__res]
      in
      [%type: [%t callback] -> [%t record_ty] -> 'sqlgg__res]
    in
    let cols_item =
      match cols_decl with None -> [] | Some d -> [ B.psig_type Recursive [ d ] ]
    in
    cols_item
    @ value (derived_name ~suffix:"of_cols_gen" tname) gen
      :: value (derived_name ~suffix:"of_cols" tname) of_cols
      :: (if nests then [] else [ value (derived_name ~suffix:"apply" tname) apply ])

let dispatch ~loc ~ext ~pick ~record tds =
  let reject ~loc msg =
    [ ext ~loc (Location.error_extensionf ~loc "deriving sqlgg: %s" msg) ]
  in

  let derive ~tname ld lds =
    let errs = ref [] in
    let err e = errs := e :: !errs in
    let first = analyze_field ~err ~pick ld in
    let rest = List.filter_map (analyze_field ~err ~pick) lds in
    match List.rev !errs, first with
    | [], Some first -> record ~loc tname first rest
    | msgs, _ -> List.concat_map (fun (loc, msg) -> reject ~loc msg) msgs
  in
  List.concat_map
    (fun td ->
      match td.ptype_params, td.ptype_kind with
      | _ :: _, _ -> reject ~loc:td.ptype_loc "type parameters are not supported"
      | [], Ptype_record [] -> reject ~loc:td.ptype_loc "record has no fields"
      | [], Ptype_record (ld :: lds) -> derive ~tname:td.ptype_name.txt ld lds
      | [], (Ptype_abstract | Ptype_variant _ | Ptype_open) ->
        reject ~loc:td.ptype_loc "only record types are supported")
    tds

let expand ~ctxt (_rec_flag, tds) =
  dispatch
    ~loc:(Expansion_context.Deriver.derived_item_loc ctxt)
    ~ext:(fun ~loc e -> D.pstr_extension ~loc e [])
    ~pick:(function Fn e -> Ok e | Raw _ -> Error "expected a conversion function")
    ~record:build tds

let expand_sig ~ctxt (_rec_flag, tds) =
  dispatch
    ~loc:(Expansion_context.Deriver.derived_item_loc ctxt)
    ~ext:(fun ~loc e -> D.psig_extension ~loc e [])
    ~pick:
      (function Raw t -> Ok t | Fn _ -> Error "expected a column type after a colon")
    ~record:sig_items tds

let () =
  Deriving.add "sqlgg"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg ~attributes expand)
    ~sig_type_decl:(Deriving.Generator.V2.make_noarg ~attributes expand_sig)
  |> Deriving.ignore
