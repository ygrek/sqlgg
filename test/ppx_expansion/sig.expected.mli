type product = {
  id: int64 ;
  name: string option }[@@deriving sqlgg]
include
  sig
    [@@@ocaml.warning "-32"]
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) product_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                            Sqlgg_scope.col  ;name: (string option,
                                                      'sqlgg__brand,
                                                      'sqlgg__row,
                                                      'sqlgg__params)
                                                      Sqlgg_scope.col   ;..
                      > 
    val product_of_cols_gen :
      id:(int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
        ->
        name:(string option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
          Sqlgg_scope.col ->
          (product, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
            Sqlgg_scope.col
    val product_of_cols :
      <
        id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col  ;name: (string option, 'sqlgg__brand,
                                        'sqlgg__row, 'sqlgg__params)
                                        Sqlgg_scope.col   ;.. > 
        ->
        (product, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
    val product_apply :
      (id:int64 -> name:string option -> 'sqlgg__res) ->
        product -> 'sqlgg__res
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type renamed = {
  id: int64 ;
  productName: string option [@sqlgg.col "name"]}[@@deriving sqlgg]
include
  sig
    [@@@ocaml.warning "-32"]
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) renamed_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                            Sqlgg_scope.col  ;name: (string option,
                                                      'sqlgg__brand,
                                                      'sqlgg__row,
                                                      'sqlgg__params)
                                                      Sqlgg_scope.col   ;..
                      > 
    val renamed_of_cols_gen :
      id:(int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
        ->
        productName:(string option, 'sqlgg__brand, 'sqlgg__row,
          'sqlgg__params) Sqlgg_scope.col ->
          (renamed, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
            Sqlgg_scope.col
    val renamed_of_cols :
      <
        id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col  ;name: (string option, 'sqlgg__brand,
                                        'sqlgg__row, 'sqlgg__params)
                                        Sqlgg_scope.col   ;.. > 
        ->
        (renamed, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
    val renamed_apply :
      (id:int64 -> name:string option -> 'sqlgg__res) ->
        renamed -> 'sqlgg__res
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type with_default = {
  id: int64 ;
  hits: int [@sqlgg.default 0]}[@@deriving sqlgg]
include
  sig
    [@@@ocaml.warning "-32"]
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) with_default_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                            Sqlgg_scope.col  ;hits: (int option,
                                                      'sqlgg__brand,
                                                      'sqlgg__row,
                                                      'sqlgg__params)
                                                      Sqlgg_scope.col   ;..
                      > 
    val with_default_of_cols_gen :
      id:(int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
        ->
        hits:(int, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
          Sqlgg_scope.col ->
          (with_default, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
            Sqlgg_scope.col
    val with_default_of_cols :
      <
        id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col  ;hits: (int option, 'sqlgg__brand,
                                        'sqlgg__row, 'sqlgg__params)
                                        Sqlgg_scope.col   ;.. > 
        ->
        (with_default, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
          Sqlgg_scope.col
    val with_default_apply :
      (id:int64 -> hits:int -> 'sqlgg__res) -> with_default -> 'sqlgg__res
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type deferred = {
  id: int64 ;
  reply_count: int [@sqlgg.by ]}[@@deriving sqlgg]
include
  sig
    [@@@ocaml.warning "-32"]
    val deferred_of_cols_gen :
      id:(int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
        ->
        reply_count:(int, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
          Sqlgg_scope.col ->
          (deferred, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
            Sqlgg_scope.col
    val deferred_of_cols :
      reply_count:('sqlgg__raw_reply_count -> int) ->
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;reply_count: ('sqlgg__raw_reply_count,
                                                 'sqlgg__brand, 'sqlgg__row,
                                                 'sqlgg__params)
                                                 Sqlgg_scope.col   ;.. > 
          ->
          (deferred, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
            Sqlgg_scope.col
    val deferred_apply :
      (id:int64 -> reply_count:int -> 'sqlgg__res) -> deferred -> 'sqlgg__res
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t = {
  id: int64 }[@@deriving sqlgg]
include
  sig
    [@@@ocaml.warning "-32"]
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params, 'sqlgg__cols) cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                            Sqlgg_scope.col   ;.. > 
    val of_cols_gen :
      id:(int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
        -> (t, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
    val of_cols :
      <
        id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col   ;.. > 
        -> (t, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
    val apply : (id:int64 -> 'sqlgg__res) -> t -> 'sqlgg__res
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type converted = {
  id: int64 ;
  reply_count: int [@sqlgg.map : int64]}[@@deriving sqlgg]
include
  sig
    [@@@ocaml.warning "-32"]
    val converted_of_cols_gen :
      id:(int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
        ->
        reply_count:(int, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
          Sqlgg_scope.col ->
          (converted, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
            Sqlgg_scope.col
    val converted_of_cols :
      <
        id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col  ;reply_count: (int64, 'sqlgg__brand,
                                               'sqlgg__row, 'sqlgg__params)
                                               Sqlgg_scope.col   ;.. > 
        ->
        (converted, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
          Sqlgg_scope.col
    val converted_apply :
      (id:int64 -> reply_count:int -> 'sqlgg__res) ->
        converted -> 'sqlgg__res
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type trimmed = {
  id: int64 ;
  name: string [@sqlgg.set : string]}[@@deriving sqlgg]
include
  sig
    [@@@ocaml.warning "-32"]
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) trimmed_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                            Sqlgg_scope.col  ;name: (string, 'sqlgg__brand,
                                                      'sqlgg__row,
                                                      'sqlgg__params)
                                                      Sqlgg_scope.col   ;..
                      > 
    val trimmed_of_cols_gen :
      id:(int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
        ->
        name:(string, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
          Sqlgg_scope.col ->
          (trimmed, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
            Sqlgg_scope.col
    val trimmed_of_cols :
      <
        id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col  ;name: (string, 'sqlgg__brand, 'sqlgg__row,
                                        'sqlgg__params) Sqlgg_scope.col   ;..
        >  ->
        (trimmed, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
    val trimmed_apply :
      (id:int64 -> name:string -> 'sqlgg__res) -> trimmed -> 'sqlgg__res
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type map_without_a_type = {
  id: int64 ;
  n: int [@sqlgg.map Int64.to_int]}[@@deriving sqlgg]
include
  sig
    [@@@ocaml.warning "-32"]
    [%%ocaml.error
      "deriving sqlgg: field n: [@sqlgg.map]: expected a column type after a colon"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type nested = {
  id: int64 ;
  product: product [@sqlgg.nested ]}[@@deriving sqlgg]
include
  sig
    [@@@ocaml.warning "-32"]
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) nested_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                            Sqlgg_scope.col   ;.. >  constraint 'sqlgg__cols
                                                      =
                                                      ('sqlgg__brand,
                                                        'sqlgg__row,
                                                        'sqlgg__params,
                                                        'sqlgg__cols)
                                                        product_cols
    val nested_of_cols_gen :
      id:(int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
        ->
        product:(product, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
          Sqlgg_scope.col ->
          (nested, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
            Sqlgg_scope.col
    val nested_of_cols :
      ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params, 'sqlgg__cols) nested_cols
        ->
        (nested, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
