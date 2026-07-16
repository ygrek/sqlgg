open Ppxlib

let fun_name = function "t" -> "of_cols" | t -> t ^ "_of_cols"

let col_attr =
  Attribute.declare "sqlgg.col" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __))
    Fun.id

let col_name ld =
  Option.value (Attribute.get col_attr ld) ~default:ld.pld_name.txt

let build ~loc tname first rest =
  let fields = first :: rest in
  let (module B) = Ast_builder.make loc in
  let open B in
  let lid = Located.lident in
  let record_ty = ptyp_constr (lid tname) [] in
  let col_ty t =
    [%type: ([%t t], 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col]
  in
  let cols_ty =
    ptyp_object
      (List.map (fun ld -> otag (Located.mk (col_name ld)) (col_ty ld.pld_type)) fields)
      Open
  in
  let record =
    pexp_record (List.map (fun ld -> lid ld.pld_name.txt, evar ld.pld_name.txt) fields) None
  in
  let bind op ld =
    { pbop_op = Located.mk op
    ; pbop_pat = pvar ld.pld_name.txt
    ; pbop_exp = pexp_send [%expr sqlgg__cols] (Located.mk (col_name ld))
    ; pbop_loc = loc
    }
  in
  let letop =
    pexp_letop
      { let_ = bind "let+" first
      ; ands = List.map (bind "and+") rest
      ; body = [%expr ([%e record] : [%t record_ty])]
      }
  in
  [%str
    let [%p pvar (fun_name tname)] =
     fun (sqlgg__cols : [%t cols_ty]) : [%t col_ty record_ty] ->
      let open Sqlgg_scope in
      [%e letop]]

let expand ~ctxt (_rec_flag, tds) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let error msg _name =
    [ Ast_builder.Default.pstr_extension ~loc
        (Location.error_extensionf ~loc "deriving sqlgg: %s" msg)
        [] ]
  in
  let build = build ~loc in
  let td_pat =
    Ast_pattern.(
      let record = map2 (ptype_record (__ ^:: __)) ~f:(fun x xs name -> build name x xs) in
      let fail msg = map1 __ ~f:(fun _ -> error msg) in
      map_value ~f:(fun td -> (td.ptype_params, td.ptype_kind), td.ptype_name.txt)
        (map1 ~f:(fun (f, name) -> f name)
          (pack2
            (pair
              (alt
                (pair nil (alt record (fail "only record types are supported")))
                (fail "type parameters are not supported"))
              __))))
  in
  List.concat_map (fun td -> Ast_pattern.parse td_pat loc td Fun.id) tds

let () =
  Deriving.add "sqlgg"
    ~str_type_decl:
      (Deriving.Generator.V2.make_noarg
         ~attributes:[ Attribute.T col_attr ]
         expand)
  |> Deriving.ignore
