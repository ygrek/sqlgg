type product = {
  id: int64 }[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : product) -> ()
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) product_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                            Sqlgg_scope.col   ;.. > 
    let product_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
          in ({ id = sqlgg__v_id } : product) : (product, 'sqlgg__brand,
                                                  'sqlgg__row,
                                                  'sqlgg__params)
                                                  Sqlgg_scope.col)
    let _ = product_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type map_and_default =
  {
  stock: int [@sqlgg.map Int64.to_int][@sqlgg.default 0]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : map_and_default) -> ()
    [%%ocaml.error
      "deriving sqlgg: field stock: [@sqlgg.map] and [@sqlgg.default] cannot be combined"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type default_on_option = {
  name: string option [@sqlgg.default "unnamed"]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : default_on_option) -> ()
    [%%ocaml.error
      "deriving sqlgg: field name: [@sqlgg.default] replaces NULL, so the field cannot be an option"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type nested_and_col =
  {
  product: product [@sqlgg.nested ][@sqlgg.col "product_id"]}[@@deriving
                                                               sqlgg]
include
  struct
    let _ = fun (_ : nested_and_col) -> ()
    [%%ocaml.error
      "deriving sqlgg: field product: [@sqlgg.nested] cannot be combined with [@sqlgg.col]"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type nested_not_a_record = {
  dimensions: (int64 * int64) [@sqlgg.nested ]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : nested_not_a_record) -> ()
    [%%ocaml.error
      "deriving sqlgg: field dimensions: [@sqlgg.nested] needs a record type deriving sqlgg"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type nested_option_not_a_record =
  {
  dimensions: (int64 * int64) option [@sqlgg.nested ]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : nested_option_not_a_record) -> ()
    [%%ocaml.error
      "deriving sqlgg: field dimensions: [@sqlgg.nested] needs a record type deriving sqlgg"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type nested_option_and_col =
  {
  product: product option [@sqlgg.nested ][@sqlgg.col "product_id"]}[@@deriving
                                                                    sqlgg]
include
  struct
    let _ = fun (_ : nested_option_and_col) -> ()
    [%%ocaml.error
      "deriving sqlgg: field product: [@sqlgg.nested] cannot be combined with [@sqlgg.col]"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type default_none_needs_option =
  {
  product: product [@sqlgg.nested default_none]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : default_none_needs_option) -> ()
    [%%ocaml.error
      "deriving sqlgg: field product: [@sqlgg.nested default_none] needs an option field"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type map_without_a_function = {
  id: int64 ;
  n: int [@sqlgg.map : int64 -> int]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : map_without_a_function) -> ()
    [%%ocaml.error
      "deriving sqlgg: field n: [@sqlgg.map]: expected a conversion function"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
