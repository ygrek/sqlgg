type product = {
  id: int64 ;
  name: string option }[@@deriving sqlgg ~nullable_cols]
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
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) product_nullable_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64 option, 'sqlgg__brand, 'sqlgg__row,
                            'sqlgg__params) Sqlgg_scope.col  ;name: (string
                                                                    option,
                                                                    'sqlgg__brand,
                                                                    'sqlgg__row,
                                                                    'sqlgg__params)
                                                                    Sqlgg_scope.col
                                                                  ;.. > 
    val product_of_cols :
      <
        id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col  ;name: (string option, 'sqlgg__brand,
                                        'sqlgg__row, 'sqlgg__params)
                                        Sqlgg_scope.col   ;.. > 
        ->
        (product, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
    val product_of_nullable_cols :
      <
        id: (int64 option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col  ;name: (string option, 'sqlgg__brand,
                                        'sqlgg__row, 'sqlgg__params)
                                        Sqlgg_scope.col   ;.. > 
        ->
        (product option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
          Sqlgg_scope.col
    val product_of_nullable_cols_exn :
      <
        id: (int64 option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col  ;name: (string option, 'sqlgg__brand,
                                        'sqlgg__row, 'sqlgg__params)
                                        Sqlgg_scope.col   ;.. > 
        ->
        (product option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
          Sqlgg_scope.col
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
    val renamed_of_cols :
      <
        id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col  ;name: (string option, 'sqlgg__brand,
                                        'sqlgg__row, 'sqlgg__params)
                                        Sqlgg_scope.col   ;.. > 
        ->
        (renamed, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
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
    val with_default_of_cols :
      <
        id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col  ;hits: (int option, 'sqlgg__brand,
                                        'sqlgg__row, 'sqlgg__params)
                                        Sqlgg_scope.col   ;.. > 
        ->
        (with_default, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
          Sqlgg_scope.col
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type deferred = {
  id: int64 ;
  reply_count: int [@sqlgg.by ]}[@@deriving sqlgg]
include
  sig
    [@@@ocaml.warning "-32"]
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
    val of_cols :
      <
        id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col   ;.. > 
        -> (t, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type converted = {
  id: int64 ;
  reply_count: int [@sqlgg.map : int64]}[@@deriving sqlgg]
include
  sig
    [@@@ocaml.warning "-32"]
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) converted_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                            Sqlgg_scope.col  ;reply_count: ('sqlgg__raw_reply_count,
                                                             'sqlgg__brand,
                                                             'sqlgg__row,
                                                             'sqlgg__params)
                                                             Sqlgg_scope.col   ;..
                      > 
    val converted_of_cols :
      <
        id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col  ;reply_count: (int64, 'sqlgg__brand,
                                               'sqlgg__row, 'sqlgg__params)
                                               Sqlgg_scope.col   ;.. > 
        ->
        (converted, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
          Sqlgg_scope.col
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type trimmed = {
  id: int64 ;
  name: string }[@@deriving sqlgg]
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
    val trimmed_of_cols :
      <
        id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col  ;name: (string, 'sqlgg__brand, 'sqlgg__row,
                                        'sqlgg__params) Sqlgg_scope.col   ;..
        >  ->
        (trimmed, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
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
    val nested_of_cols :
      ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params, 'sqlgg__cols) nested_cols
        ->
        (nested, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type optional_relation =
  {
  id: int64 ;
  product: product option [@sqlgg.nested ]}[@@deriving sqlgg]
include
  sig
    [@@@ocaml.warning "-32"]
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) optional_relation_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                            Sqlgg_scope.col   ;.. >  constraint 'sqlgg__cols
                                                      =
                                                      ('sqlgg__brand,
                                                        'sqlgg__row,
                                                        'sqlgg__params,
                                                        'sqlgg__cols)
                                                        product_nullable_cols
    val optional_relation_of_cols :
      ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params, 'sqlgg__cols)
        optional_relation_cols ->
        (optional_relation, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
          Sqlgg_scope.col
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type all_option = {
  note: string option }[@@deriving sqlgg]
include
  sig
    [@@@ocaml.warning "-32"]
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) all_option_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      note: (string option, 'sqlgg__brand, 'sqlgg__row,
                              'sqlgg__params) Sqlgg_scope.col   ;.. > 
    val all_option_of_cols :
      <
        note: (string option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col   ;.. > 
        ->
        (all_option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
          Sqlgg_scope.col
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type with_nested = {
  a: int64 ;
  product: product [@sqlgg.nested ]}[@@deriving sqlgg ~nullable_cols]
include
  sig
    [@@@ocaml.warning "-32"]
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) with_nested_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      a: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                           Sqlgg_scope.col   ;.. >  constraint 'sqlgg__cols =
                                                     ('sqlgg__brand,
                                                       'sqlgg__row,
                                                       'sqlgg__params,
                                                       'sqlgg__cols)
                                                       product_cols
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) with_nested_nullable_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      a: (int64 option, 'sqlgg__brand, 'sqlgg__row,
                           'sqlgg__params) Sqlgg_scope.col   ;.. > 
                                                                    constraint
                                                                    'sqlgg__cols
                                                                    =
                                                                    ('sqlgg__brand,
                                                                    'sqlgg__row,
                                                                    'sqlgg__params,
                                                                    'sqlgg__cols)
                                                                    product_nullable_cols
    val with_nested_of_cols :
      ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params, 'sqlgg__cols)
        with_nested_cols ->
        (with_nested, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
          Sqlgg_scope.col
    val with_nested_of_nullable_cols :
      ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params, 'sqlgg__cols)
        with_nested_nullable_cols ->
        (with_nested option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
          Sqlgg_scope.col
    val with_nested_of_nullable_cols_exn :
      ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params, 'sqlgg__cols)
        with_nested_nullable_cols ->
        (with_nested option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
          Sqlgg_scope.col
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type nested_in_relation =
  {
  id: int64 ;
  inner: with_nested option [@sqlgg.nested ]}[@@deriving sqlgg]
include
  sig
    [@@@ocaml.warning "-32"]
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) nested_in_relation_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                            Sqlgg_scope.col   ;.. >  constraint 'sqlgg__cols
                                                      =
                                                      ('sqlgg__brand,
                                                        'sqlgg__row,
                                                        'sqlgg__params,
                                                        'sqlgg__cols)
                                                        with_nested_nullable_cols
    val nested_in_relation_of_cols :
      ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params, 'sqlgg__cols)
        nested_in_relation_cols ->
        (nested_in_relation, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
          Sqlgg_scope.col
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type relation_and_conversions =
  {
  id: int64 ;
  product: product option [@sqlgg.nested ];
  reply_count: int [@sqlgg.by ]}[@@deriving sqlgg]
include
  sig
    [@@@ocaml.warning "-32"]
    [%%ocaml.error
      "deriving sqlgg: [@sqlgg.nested] needs relation_and_conversions_cols, and the parent cannot pass the [@sqlgg.by] argument"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
