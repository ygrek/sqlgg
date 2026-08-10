type product = {
  id: int64 ;
  name: string option }[@@deriving sqlgg ~nullable_cols]
include
  struct
    let _ = fun (_ : product) -> ()
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
    let product_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;name: (string option, 'sqlgg__brand,
                                          'sqlgg__row, 'sqlgg__params)
                                          Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_name = sqlgg__cols#name in
         ({ id = sqlgg__v_id; name = sqlgg__v_name } : product) : (product,
                                                                    'sqlgg__brand,
                                                                    'sqlgg__row,
                                                                    'sqlgg__params)
                                                                    Sqlgg_scope.col)
    let _ = product_of_cols
    let product_of_nullable_cols
      (sqlgg__cols :
        <
          id: (int64 option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;name: (string option, 'sqlgg__brand,
                                          'sqlgg__row, 'sqlgg__params)
                                          Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_name = sqlgg__cols#name in
         Stdlib.Option.bind sqlgg__v_id
           (fun sqlgg__v_id ->
              Some ({ id = sqlgg__v_id; name = sqlgg__v_name } : product)) : 
      (product option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
        Sqlgg_scope.col)
    let _ = product_of_nullable_cols
    let product_of_nullable_cols_exn
      (sqlgg__cols :
        <
          id: (int64 option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;name: (string option, 'sqlgg__brand,
                                          'sqlgg__row, 'sqlgg__params)
                                          Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_name = sqlgg__cols#name in
         match (sqlgg__v_id, sqlgg__v_name) with
         | (Some sqlgg__v_id, _) ->
             Some ({ id = sqlgg__v_id; name = sqlgg__v_name } : product)
         | (None, None) -> None
         | (None, _) -> failwith "sqlgg: product.id is NULL" : (product
                                                                  option,
                                                                 'sqlgg__brand,
                                                                 'sqlgg__row,
                                                                 'sqlgg__params)
                                                                 Sqlgg_scope.col)
    let _ = product_of_nullable_cols_exn
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type renamed = {
  id: int64 ;
  productName: string option [@sqlgg.col "name"]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : renamed) -> ()
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
    let renamed_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;name: (string option, 'sqlgg__brand,
                                          'sqlgg__row, 'sqlgg__params)
                                          Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_productName = sqlgg__cols#name in
         ({ id = sqlgg__v_id; productName = sqlgg__v_productName } : 
           renamed) : (renamed, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                        Sqlgg_scope.col)
    let _ = renamed_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type same_column_twice = {
  id: int64 ;
  also_id: int64 [@sqlgg.col "id"]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : same_column_twice) -> ()
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) same_column_twice_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                            Sqlgg_scope.col  ;id: (int64, 'sqlgg__brand,
                                                    'sqlgg__row,
                                                    'sqlgg__params)
                                                    Sqlgg_scope.col   ;.. > 
    let same_column_twice_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;id: (int64, 'sqlgg__brand, 'sqlgg__row,
                                        'sqlgg__params) Sqlgg_scope.col   ;..
          > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_also_id = sqlgg__cols#id in
         ({ id = sqlgg__v_id; also_id = sqlgg__v_also_id } : same_column_twice) : 
      (same_column_twice, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
        Sqlgg_scope.col)
    let _ = same_column_twice_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type converted = {
  id: int64 ;
  reply_count: int [@sqlgg.map Int64.to_int]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : converted) -> ()
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
    let converted_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;reply_count: ('sqlgg__raw_reply_count,
                                                 'sqlgg__brand, 'sqlgg__row,
                                                 'sqlgg__params)
                                                 Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_reply_count = sqlgg__cols#reply_count in
         ({
            id = sqlgg__v_id;
            reply_count = (Int64.to_int sqlgg__v_reply_count)
          } : converted) : (converted, 'sqlgg__brand, 'sqlgg__row,
                             'sqlgg__params) Sqlgg_scope.col)
    let _ = converted_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type deferred = {
  id: int64 ;
  reply_count: int [@sqlgg.by ]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : deferred) -> ()
    let deferred_of_cols ~reply_count:sqlgg__conv_reply_count
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;reply_count: ('sqlgg__raw_reply_count,
                                                 'sqlgg__brand, 'sqlgg__row,
                                                 'sqlgg__params)
                                                 Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_reply_count = sqlgg__cols#reply_count in
         ({
            id = sqlgg__v_id;
            reply_count = (sqlgg__conv_reply_count sqlgg__v_reply_count)
          } : deferred) : (deferred, 'sqlgg__brand, 'sqlgg__row,
                            'sqlgg__params) Sqlgg_scope.col)
    let _ = deferred_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type deferred_with_default =
  {
  id: int64 ;
  reply_count: int [@sqlgg.by ][@sqlgg.map Int64.to_int]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : deferred_with_default) -> ()
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) deferred_with_default_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                            Sqlgg_scope.col  ;reply_count: ('sqlgg__raw_reply_count,
                                                             'sqlgg__brand,
                                                             'sqlgg__row,
                                                             'sqlgg__params)
                                                             Sqlgg_scope.col   ;..
                      > 
    let deferred_with_default_of_cols ?reply_count:(sqlgg__conv_reply_count=
      Int64.to_int)
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;reply_count: ('sqlgg__raw_reply_count,
                                                 'sqlgg__brand, 'sqlgg__row,
                                                 'sqlgg__params)
                                                 Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_reply_count = sqlgg__cols#reply_count in
         ({
            id = sqlgg__v_id;
            reply_count = (sqlgg__conv_reply_count sqlgg__v_reply_count)
          } : deferred_with_default) : (deferred_with_default, 'sqlgg__brand,
                                         'sqlgg__row, 'sqlgg__params)
                                         Sqlgg_scope.col)
    let _ = deferred_with_default_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type with_default = {
  id: int64 ;
  hits: int [@sqlgg.default 0]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : with_default) -> ()
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
    let with_default_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;hits: (int option, 'sqlgg__brand,
                                          'sqlgg__row, 'sqlgg__params)
                                          Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_hits = sqlgg__cols#hits in
         ({
            id = sqlgg__v_id;
            hits = ((Stdlib.Option.value ~default:0) sqlgg__v_hits)
          } : with_default) : (with_default, 'sqlgg__brand, 'sqlgg__row,
                                'sqlgg__params) Sqlgg_scope.col)
    let _ = with_default_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type trimmed = {
  id: int64 ;
  name: string }[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : trimmed) -> ()
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
    let trimmed_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;name: (string, 'sqlgg__brand, 'sqlgg__row,
                                          'sqlgg__params) Sqlgg_scope.col   ;..
          > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_name = sqlgg__cols#name in
         ({ id = sqlgg__v_id; name = sqlgg__v_name } : trimmed) : (trimmed,
                                                                    'sqlgg__brand,
                                                                    'sqlgg__row,
                                                                    'sqlgg__params)
                                                                    Sqlgg_scope.col)
    let _ = trimmed_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type nested = {
  id: int64 ;
  product: product [@sqlgg.nested ]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : nested) -> ()
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
    let nested_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_product = product_of_cols sqlgg__cols in
         ({ id = sqlgg__v_id; product = sqlgg__v_product } : nested) : 
      (nested, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col)
    let _ = nested_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type nested_qualified = {
  id: int64 ;
  channel: Feed.channel [@sqlgg.nested ]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : nested_qualified) -> ()
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) nested_qualified_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                            Sqlgg_scope.col   ;.. >  constraint 'sqlgg__cols
                                                      =
                                                      ('sqlgg__brand,
                                                        'sqlgg__row,
                                                        'sqlgg__params,
                                                        'sqlgg__cols)
                                                        Feed.channel_cols
    let nested_qualified_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_channel = Feed.channel_of_cols sqlgg__cols in
         ({ id = sqlgg__v_id; channel = sqlgg__v_channel } : nested_qualified) : 
      (nested_qualified, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
        Sqlgg_scope.col)
    let _ = nested_qualified_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type optional_relation =
  {
  id: int64 ;
  product: product option [@sqlgg.nested ]}[@@deriving sqlgg ~nullable_cols]
include
  struct
    let _ = fun (_ : optional_relation) -> ()
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
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) optional_relation_nullable_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64 option, 'sqlgg__brand, 'sqlgg__row,
                            'sqlgg__params) Sqlgg_scope.col   ;.. > 
                                                                    constraint
                                                                    'sqlgg__cols
                                                                    =
                                                                    ('sqlgg__brand,
                                                                    'sqlgg__row,
                                                                    'sqlgg__params,
                                                                    'sqlgg__cols)
                                                                    product_nullable_cols
    let optional_relation_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_product = product_of_nullable_cols_exn sqlgg__cols in
         ({ id = sqlgg__v_id; product = sqlgg__v_product } : optional_relation) : 
      (optional_relation, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
        Sqlgg_scope.col)
    let _ = optional_relation_of_cols
    let optional_relation_of_nullable_cols
      (sqlgg__cols :
        <
          id: (int64 option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_product = product_of_nullable_cols sqlgg__cols in
         Stdlib.Option.bind sqlgg__v_id
           (fun sqlgg__v_id ->
              Some
                ({ id = sqlgg__v_id; product = sqlgg__v_product } : optional_relation)) : 
      (optional_relation option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
        Sqlgg_scope.col)
    let _ = optional_relation_of_nullable_cols
    let optional_relation_of_nullable_cols_exn
      (sqlgg__cols :
        <
          id: (int64 option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_product = product_of_nullable_cols_exn sqlgg__cols in
         match (sqlgg__v_id, sqlgg__v_product) with
         | (Some sqlgg__v_id, _) ->
             Some
               ({ id = sqlgg__v_id; product = sqlgg__v_product } : optional_relation)
         | (None, None) -> None
         | (None, _) -> failwith "sqlgg: optional_relation.id is NULL" : 
      (optional_relation option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
        Sqlgg_scope.col)
    let _ = optional_relation_of_nullable_cols_exn
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type relation_qualified =
  {
  id: int64 ;
  channel: Feed.channel option [@sqlgg.nested ]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : relation_qualified) -> ()
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) relation_qualified_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                            Sqlgg_scope.col   ;.. >  constraint 'sqlgg__cols
                                                      =
                                                      ('sqlgg__brand,
                                                        'sqlgg__row,
                                                        'sqlgg__params,
                                                        'sqlgg__cols)
                                                        Feed.channel_nullable_cols
    let relation_qualified_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_channel =
           Feed.channel_of_nullable_cols_exn sqlgg__cols in
         ({ id = sqlgg__v_id; channel = sqlgg__v_channel } : relation_qualified) : 
      (relation_qualified, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
        Sqlgg_scope.col)
    let _ = relation_qualified_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type relation_and_conversions =
  {
  id: int64 ;
  product: product option [@sqlgg.nested ];
  reply_count: int [@sqlgg.map Int64.to_int];
  hits: int [@sqlgg.default 0]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : relation_and_conversions) -> ()
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) relation_and_conversions_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                            Sqlgg_scope.col  ;reply_count: ('sqlgg__raw_reply_count,
                                                             'sqlgg__brand,
                                                             'sqlgg__row,
                                                             'sqlgg__params)
                                                             Sqlgg_scope.col  ;
                      hits: (int option, 'sqlgg__brand, 'sqlgg__row,
                              'sqlgg__params) Sqlgg_scope.col   ;.. > 
       constraint 'sqlgg__cols =
        ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params, 'sqlgg__cols)
          product_nullable_cols
    let relation_and_conversions_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;reply_count: ('sqlgg__raw_reply_count,
                                                 'sqlgg__brand, 'sqlgg__row,
                                                 'sqlgg__params)
                                                 Sqlgg_scope.col  ;hits: 
                                                                    (int
                                                                    option,
                                                                    'sqlgg__brand,
                                                                    'sqlgg__row,
                                                                    'sqlgg__params)
                                                                    Sqlgg_scope.col
                                                                      ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_product = product_of_nullable_cols_exn sqlgg__cols
         and+ sqlgg__v_reply_count = sqlgg__cols#reply_count
         and+ sqlgg__v_hits = sqlgg__cols#hits in
         ({
            id = sqlgg__v_id;
            product = sqlgg__v_product;
            reply_count = (Int64.to_int sqlgg__v_reply_count);
            hits = ((Stdlib.Option.value ~default:0) sqlgg__v_hits)
          } : relation_and_conversions) : (relation_and_conversions,
                                            'sqlgg__brand, 'sqlgg__row,
                                            'sqlgg__params) Sqlgg_scope.col)
    let _ = relation_and_conversions_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type all_option = {
  note: string option ;
  tag: string option }[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : all_option) -> ()
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) all_option_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      note: (string option, 'sqlgg__brand, 'sqlgg__row,
                              'sqlgg__params) Sqlgg_scope.col  ;tag: 
                                                                  (string
                                                                    option,
                                                                    'sqlgg__brand,
                                                                    'sqlgg__row,
                                                                    'sqlgg__params)
                                                                    Sqlgg_scope.col
                                                                    ;.. > 
    let all_option_of_cols
      (sqlgg__cols :
        <
          note: (string option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                  Sqlgg_scope.col  ;tag: (string option, 'sqlgg__brand,
                                           'sqlgg__row, 'sqlgg__params)
                                           Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_note = sqlgg__cols#note
         and+ sqlgg__v_tag = sqlgg__cols#tag in
         ({ note = sqlgg__v_note; tag = sqlgg__v_tag } : all_option) : 
      (all_option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
        Sqlgg_scope.col)
    let _ = all_option_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type opaque_conversions =
  {
  id: int64 ;
  note: string option [@sqlgg.map Fun.id];
  tag: int option [@sqlgg.by ]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : opaque_conversions) -> ()
    let opaque_conversions_of_cols ~tag:sqlgg__conv_tag
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;note: ('sqlgg__raw_note, 'sqlgg__brand,
                                          'sqlgg__row, 'sqlgg__params)
                                          Sqlgg_scope.col  ;tag: ('sqlgg__raw_tag,
                                                                   'sqlgg__brand,
                                                                   'sqlgg__row,
                                                                   'sqlgg__params)
                                                                   Sqlgg_scope.col
                                                                ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_note = sqlgg__cols#note
         and+ sqlgg__v_tag = sqlgg__cols#tag in
         ({
            id = sqlgg__v_id;
            note = (Fun.id sqlgg__v_note);
            tag = (sqlgg__conv_tag sqlgg__v_tag)
          } : opaque_conversions) : (opaque_conversions, 'sqlgg__brand,
                                      'sqlgg__row, 'sqlgg__params)
                                      Sqlgg_scope.col)
    let _ = opaque_conversions_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t = {
  id: int64 ;
  label: string [@sqlgg.col "name"]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : t) -> ()
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params, 'sqlgg__cols) cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                            Sqlgg_scope.col  ;name: (string, 'sqlgg__brand,
                                                      'sqlgg__row,
                                                      'sqlgg__params)
                                                      Sqlgg_scope.col   ;..
                      > 
    let of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;name: (string, 'sqlgg__brand, 'sqlgg__row,
                                          'sqlgg__params) Sqlgg_scope.col   ;..
          > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_label = sqlgg__cols#name in
         ({ id = sqlgg__v_id; label = sqlgg__v_label } : t) : (t,
                                                                'sqlgg__brand,
                                                                'sqlgg__row,
                                                                'sqlgg__params)
                                                                Sqlgg_scope.col)
    let _ = of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type variant =
  | A 
  | B [@@deriving sqlgg]
include
  struct
    let _ = fun (_ : variant) -> ()
    [%%ocaml.error "deriving sqlgg: only record types are supported"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type 'a parameterised = {
  id: 'a }[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : 'a parameterised) -> ()
    [%%ocaml.error "deriving sqlgg: type parameters are not supported"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type default_none_relation =
  {
  id: int64 ;
  product: product option [@sqlgg.nested default_none]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : default_none_relation) -> ()
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) default_none_relation_cols =
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
    let default_none_relation_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_product = product_of_nullable_cols sqlgg__cols in
         ({ id = sqlgg__v_id; product = sqlgg__v_product } : default_none_relation) : 
      (default_none_relation, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
        Sqlgg_scope.col)
    let _ = default_none_relation_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type with_nested = {
  a: int64 ;
  product: product [@sqlgg.nested ]}[@@deriving sqlgg ~nullable_cols]
include
  struct
    let _ = fun (_ : with_nested) -> ()
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
    let with_nested_of_cols
      (sqlgg__cols :
        <
          a: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
               Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_a = sqlgg__cols#a
         and+ sqlgg__v_product = product_of_cols sqlgg__cols in
         ({ a = sqlgg__v_a; product = sqlgg__v_product } : with_nested) : 
      (with_nested, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
        Sqlgg_scope.col)
    let _ = with_nested_of_cols
    let with_nested_of_nullable_cols
      (sqlgg__cols :
        <
          a: (int64 option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
               Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_a = sqlgg__cols#a
         and+ sqlgg__v_product = product_of_nullable_cols sqlgg__cols in
         Stdlib.Option.bind sqlgg__v_a
           (fun sqlgg__v_a ->
              Stdlib.Option.bind sqlgg__v_product
                (fun sqlgg__v_product ->
                   Some
                     ({ a = sqlgg__v_a; product = sqlgg__v_product } : 
                     with_nested))) : (with_nested option, 'sqlgg__brand,
                                        'sqlgg__row, 'sqlgg__params)
                                        Sqlgg_scope.col)
    let _ = with_nested_of_nullable_cols
    let with_nested_of_nullable_cols_exn
      (sqlgg__cols :
        <
          a: (int64 option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
               Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_a = sqlgg__cols#a
         and+ sqlgg__v_product = product_of_nullable_cols sqlgg__cols in
         match (sqlgg__v_a, sqlgg__v_product) with
         | (Some sqlgg__v_a, Some sqlgg__v_product) ->
             Some
               ({ a = sqlgg__v_a; product = sqlgg__v_product } : with_nested)
         | (None, None) -> None
         | (None, _) -> failwith "sqlgg: with_nested.a is NULL"
         | (_, None) -> failwith "sqlgg: with_nested.product is NULL" : 
      (with_nested option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
        Sqlgg_scope.col)
    let _ = with_nested_of_nullable_cols_exn
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type nested_in_relation =
  {
  id: int64 ;
  inner: with_nested option [@sqlgg.nested ]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : nested_in_relation) -> ()
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
    let nested_in_relation_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_inner = with_nested_of_nullable_cols_exn sqlgg__cols in
         ({ id = sqlgg__v_id; inner = sqlgg__v_inner } : nested_in_relation) : 
      (nested_in_relation, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
        Sqlgg_scope.col)
    let _ = nested_in_relation_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type chains_to_optional =
  {
  id: int64 ;
  rel: optional_relation [@sqlgg.nested ]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : chains_to_optional) -> ()
    type ('sqlgg__brand, 'sqlgg__row, 'sqlgg__params,
      'sqlgg__cols) chains_to_optional_cols =
      'sqlgg__cols constraint 'sqlgg__cols =
                    <
                      id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                            Sqlgg_scope.col   ;.. >  constraint 'sqlgg__cols
                                                      =
                                                      ('sqlgg__brand,
                                                        'sqlgg__row,
                                                        'sqlgg__params,
                                                        'sqlgg__cols)
                                                        optional_relation_cols
    let chains_to_optional_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col   ;.. > )
      =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__cols#id
         and+ sqlgg__v_rel = optional_relation_of_cols sqlgg__cols in
         ({ id = sqlgg__v_id; rel = sqlgg__v_rel } : chains_to_optional) : 
      (chains_to_optional, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
        Sqlgg_scope.col)
    let _ = chains_to_optional_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
