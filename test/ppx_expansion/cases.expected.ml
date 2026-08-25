type product = {
  id: int64 ;
  name: string option }[@@deriving sqlgg]
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
    let product_of_cols_gen
      ~id:(sqlgg__c_id :
            (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col)
       ~name:(sqlgg__c_name :
               (string option, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                 Sqlgg_scope.col)
       =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__c_id
         and+ sqlgg__v_name = sqlgg__c_name in
         ({ id = sqlgg__v_id; name = sqlgg__v_name } : product) : (product,
                                                                    'sqlgg__brand,
                                                                    'sqlgg__row,
                                                                    'sqlgg__params)
                                                                    Sqlgg_scope.col)
    let _ = product_of_cols_gen
    let product_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;name: (string option, 'sqlgg__brand,
                                          'sqlgg__row, 'sqlgg__params)
                                          Sqlgg_scope.col   ;.. > )
      =
      (product_of_cols_gen ~id:(sqlgg__cols#id) ~name:(sqlgg__cols#name) : 
      (product, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col)
    let _ = product_of_cols
    let product_apply sqlgg__f (sqlgg__r : product) =
      sqlgg__f ~id:(sqlgg__r.id) ~name:(sqlgg__r.name)
    let _ = product_apply
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
    let renamed_of_cols_gen
      ~id:(sqlgg__c_id :
            (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col)
       ~productName:(sqlgg__c_productName :
                      (string option, 'sqlgg__brand, 'sqlgg__row,
                        'sqlgg__params) Sqlgg_scope.col)
       =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__c_id
         and+ sqlgg__v_productName = sqlgg__c_productName in
         ({ id = sqlgg__v_id; productName = sqlgg__v_productName } : 
           renamed) : (renamed, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                        Sqlgg_scope.col)
    let _ = renamed_of_cols_gen
    let renamed_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;name: (string option, 'sqlgg__brand,
                                          'sqlgg__row, 'sqlgg__params)
                                          Sqlgg_scope.col   ;.. > )
      =
      (renamed_of_cols_gen ~id:(sqlgg__cols#id)
         ~productName:(sqlgg__cols#name) : (renamed, 'sqlgg__brand,
                                             'sqlgg__row, 'sqlgg__params)
                                             Sqlgg_scope.col)
    let _ = renamed_of_cols
    let renamed_apply sqlgg__f (sqlgg__r : renamed) =
      sqlgg__f ~id:(sqlgg__r.id) ~name:(sqlgg__r.productName)
    let _ = renamed_apply
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
    let same_column_twice_of_cols_gen
      ~id:(sqlgg__c_id :
            (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col)
       ~also_id:(sqlgg__c_also_id :
                  (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                    Sqlgg_scope.col)
       =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__c_id
         and+ sqlgg__v_also_id = sqlgg__c_also_id in
         ({ id = sqlgg__v_id; also_id = sqlgg__v_also_id } : same_column_twice) : 
      (same_column_twice, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
        Sqlgg_scope.col)
    let _ = same_column_twice_of_cols_gen
    let same_column_twice_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;id: (int64, 'sqlgg__brand, 'sqlgg__row,
                                        'sqlgg__params) Sqlgg_scope.col   ;..
          > )
      =
      (same_column_twice_of_cols_gen ~id:(sqlgg__cols#id)
         ~also_id:(sqlgg__cols#id) : (same_column_twice, 'sqlgg__brand,
                                       'sqlgg__row, 'sqlgg__params)
                                       Sqlgg_scope.col)
    let _ = same_column_twice_of_cols
    let same_column_twice_apply sqlgg__f (sqlgg__r : same_column_twice) =
      sqlgg__f ~id:(sqlgg__r.id) ~id:(sqlgg__r.also_id)
    let _ = same_column_twice_apply
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type converted = {
  id: int64 ;
  reply_count: int [@sqlgg.map Int64.to_int]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : converted) -> ()
    let converted_of_cols_gen
      ~id:(sqlgg__c_id :
            (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col)
       ~reply_count:(sqlgg__c_reply_count :
                      (int, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                        Sqlgg_scope.col)
       =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__c_id
         and+ sqlgg__v_reply_count = sqlgg__c_reply_count in
         ({ id = sqlgg__v_id; reply_count = sqlgg__v_reply_count } : 
           converted) : (converted, 'sqlgg__brand, 'sqlgg__row,
                          'sqlgg__params) Sqlgg_scope.col)
    let _ = converted_of_cols_gen
    let converted_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;reply_count: ('sqlgg__raw_reply_count,
                                                 'sqlgg__brand, 'sqlgg__row,
                                                 'sqlgg__params)
                                                 Sqlgg_scope.col   ;.. > )
      =
      (converted_of_cols_gen ~id:(sqlgg__cols#id)
         ~reply_count:(Sqlgg_scope.map Int64.to_int sqlgg__cols#reply_count) : 
      (converted, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col)
    let _ = converted_of_cols
    let converted_apply sqlgg__f (sqlgg__r : converted) =
      sqlgg__f ~id:(sqlgg__r.id) ~reply_count:(sqlgg__r.reply_count)
    let _ = converted_apply
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type deferred = {
  id: int64 ;
  reply_count: int [@sqlgg.by ]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : deferred) -> ()
    let deferred_of_cols_gen
      ~id:(sqlgg__c_id :
            (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col)
       ~reply_count:(sqlgg__c_reply_count :
                      (int, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                        Sqlgg_scope.col)
       =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__c_id
         and+ sqlgg__v_reply_count = sqlgg__c_reply_count in
         ({ id = sqlgg__v_id; reply_count = sqlgg__v_reply_count } : 
           deferred) : (deferred, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                         Sqlgg_scope.col)
    let _ = deferred_of_cols_gen
    let deferred_of_cols ~reply_count:sqlgg__conv_reply_count 
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;reply_count: ('sqlgg__raw_reply_count,
                                                 'sqlgg__brand, 'sqlgg__row,
                                                 'sqlgg__params)
                                                 Sqlgg_scope.col   ;.. > )
      =
      (deferred_of_cols_gen ~id:(sqlgg__cols#id)
         ~reply_count:(Sqlgg_scope.map sqlgg__conv_reply_count
                         sqlgg__cols#reply_count) : (deferred, 'sqlgg__brand,
                                                      'sqlgg__row,
                                                      'sqlgg__params)
                                                      Sqlgg_scope.col)
    let _ = deferred_of_cols
    let deferred_apply sqlgg__f (sqlgg__r : deferred) =
      sqlgg__f ~id:(sqlgg__r.id) ~reply_count:(sqlgg__r.reply_count)
    let _ = deferred_apply
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type deferred_with_default =
  {
  id: int64 ;
  reply_count: int [@sqlgg.by ][@sqlgg.map Int64.to_int]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : deferred_with_default) -> ()
    let deferred_with_default_of_cols_gen
      ~id:(sqlgg__c_id :
            (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col)
       ~reply_count:(sqlgg__c_reply_count :
                      (int, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                        Sqlgg_scope.col)
       =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__c_id
         and+ sqlgg__v_reply_count = sqlgg__c_reply_count in
         ({ id = sqlgg__v_id; reply_count = sqlgg__v_reply_count } : 
           deferred_with_default) : (deferred_with_default, 'sqlgg__brand,
                                      'sqlgg__row, 'sqlgg__params)
                                      Sqlgg_scope.col)
    let _ = deferred_with_default_of_cols_gen
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
      (deferred_with_default_of_cols_gen ~id:(sqlgg__cols#id)
         ~reply_count:(Sqlgg_scope.map sqlgg__conv_reply_count
                         sqlgg__cols#reply_count) : (deferred_with_default,
                                                      'sqlgg__brand,
                                                      'sqlgg__row,
                                                      'sqlgg__params)
                                                      Sqlgg_scope.col)
    let _ = deferred_with_default_of_cols
    let deferred_with_default_apply sqlgg__f
      (sqlgg__r : deferred_with_default) =
      sqlgg__f ~id:(sqlgg__r.id) ~reply_count:(sqlgg__r.reply_count)
    let _ = deferred_with_default_apply
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
    let with_default_of_cols_gen
      ~id:(sqlgg__c_id :
            (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col)
       ~hits:(sqlgg__c_hits :
               (int, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                 Sqlgg_scope.col)
       =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__c_id
         and+ sqlgg__v_hits = sqlgg__c_hits in
         ({ id = sqlgg__v_id; hits = sqlgg__v_hits } : with_default) : 
      (with_default, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
        Sqlgg_scope.col)
    let _ = with_default_of_cols_gen
    let with_default_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;hits: (int option, 'sqlgg__brand,
                                          'sqlgg__row, 'sqlgg__params)
                                          Sqlgg_scope.col   ;.. > )
      =
      (with_default_of_cols_gen ~id:(sqlgg__cols#id)
         ~hits:(Sqlgg_scope.map (Stdlib.Option.value ~default:0)
                  sqlgg__cols#hits) : (with_default, 'sqlgg__brand,
                                        'sqlgg__row, 'sqlgg__params)
                                        Sqlgg_scope.col)
    let _ = with_default_of_cols
    let with_default_apply sqlgg__f (sqlgg__r : with_default) =
      sqlgg__f ~id:(sqlgg__r.id) ~hits:(sqlgg__r.hits)
    let _ = with_default_apply
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type trimmed = {
  id: int64 ;
  name: string [@sqlgg.set String.trim]}[@@deriving sqlgg]
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
    let trimmed_of_cols_gen
      ~id:(sqlgg__c_id :
            (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col)
       ~name:(sqlgg__c_name :
               (string, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                 Sqlgg_scope.col)
       =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__c_id
         and+ sqlgg__v_name = sqlgg__c_name in
         ({ id = sqlgg__v_id; name = sqlgg__v_name } : trimmed) : (trimmed,
                                                                    'sqlgg__brand,
                                                                    'sqlgg__row,
                                                                    'sqlgg__params)
                                                                    Sqlgg_scope.col)
    let _ = trimmed_of_cols_gen
    let trimmed_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;name: (string, 'sqlgg__brand, 'sqlgg__row,
                                          'sqlgg__params) Sqlgg_scope.col   ;..
          > )
      =
      (trimmed_of_cols_gen ~id:(sqlgg__cols#id) ~name:(sqlgg__cols#name) : 
      (trimmed, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col)
    let _ = trimmed_of_cols
    let trimmed_apply sqlgg__f (sqlgg__r : trimmed) =
      sqlgg__f ~id:(sqlgg__r.id) ~name:(String.trim sqlgg__r.name)
    let _ = trimmed_apply
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
    let nested_of_cols_gen
      ~id:(sqlgg__c_id :
            (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col)
       ~product:(sqlgg__c_product :
                  (product, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                    Sqlgg_scope.col)
       =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__c_id
         and+ sqlgg__v_product = sqlgg__c_product in
         ({ id = sqlgg__v_id; product = sqlgg__v_product } : nested) : 
      (nested, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col)
    let _ = nested_of_cols_gen
    let nested_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col   ;.. > )
      =
      (nested_of_cols_gen ~id:(sqlgg__cols#id)
         ~product:(product_of_cols sqlgg__cols) : (nested, 'sqlgg__brand,
                                                    'sqlgg__row,
                                                    'sqlgg__params)
                                                    Sqlgg_scope.col)
    let _ = nested_of_cols
    let nested_apply sqlgg__f (sqlgg__r : nested) =
      product_apply (sqlgg__f ~id:(sqlgg__r.id)) sqlgg__r.product
    let _ = nested_apply
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
    let nested_qualified_of_cols_gen
      ~id:(sqlgg__c_id :
            (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col)
       ~channel:(sqlgg__c_channel :
                  (Feed.channel, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                    Sqlgg_scope.col)
       =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__c_id
         and+ sqlgg__v_channel = sqlgg__c_channel in
         ({ id = sqlgg__v_id; channel = sqlgg__v_channel } : nested_qualified) : 
      (nested_qualified, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
        Sqlgg_scope.col)
    let _ = nested_qualified_of_cols_gen
    let nested_qualified_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col   ;.. > )
      =
      (nested_qualified_of_cols_gen ~id:(sqlgg__cols#id)
         ~channel:(Feed.channel_of_cols sqlgg__cols) : (nested_qualified,
                                                         'sqlgg__brand,
                                                         'sqlgg__row,
                                                         'sqlgg__params)
                                                         Sqlgg_scope.col)
    let _ = nested_qualified_of_cols
    let nested_qualified_apply sqlgg__f (sqlgg__r : nested_qualified) =
      Feed.channel_apply (sqlgg__f ~id:(sqlgg__r.id)) sqlgg__r.channel
    let _ = nested_qualified_apply
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
    let of_cols_gen
      ~id:(sqlgg__c_id :
            (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
              Sqlgg_scope.col)
       ~label:(sqlgg__c_label :
                (string, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                  Sqlgg_scope.col)
       =
      (let open Sqlgg_scope in
         let+ sqlgg__v_id = sqlgg__c_id
         and+ sqlgg__v_label = sqlgg__c_label in
         ({ id = sqlgg__v_id; label = sqlgg__v_label } : t) : (t,
                                                                'sqlgg__brand,
                                                                'sqlgg__row,
                                                                'sqlgg__params)
                                                                Sqlgg_scope.col)
    let _ = of_cols_gen
    let of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;name: (string, 'sqlgg__brand, 'sqlgg__row,
                                          'sqlgg__params) Sqlgg_scope.col   ;..
          > )
      =
      (of_cols_gen ~id:(sqlgg__cols#id) ~label:(sqlgg__cols#name) : (t,
                                                                    'sqlgg__brand,
                                                                    'sqlgg__row,
                                                                    'sqlgg__params)
                                                                    Sqlgg_scope.col)
    let _ = of_cols
    let apply sqlgg__f (sqlgg__r : t) =
      sqlgg__f ~id:(sqlgg__r.id) ~name:(sqlgg__r.label)
    let _ = apply
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
