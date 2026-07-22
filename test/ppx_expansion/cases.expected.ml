type who = {
  id: int64 ;
  name: string option }[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : who) -> ()
    let who_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;name: (string option, 'sqlgg__brand,
                                          'sqlgg__row, 'sqlgg__params)
                                          Sqlgg_scope.col   ;.. > )
      : (who, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col=
      let open Sqlgg_scope in
        let+ id = sqlgg__cols#id
        and+ name = sqlgg__cols#name in ({ id; name } : who)
    let _ = who_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type renamed = {
  id: int64 ;
  productName: string option [@sqlgg.col "name"]}[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : renamed) -> ()
    let renamed_of_cols
      (sqlgg__cols :
        <
          id: (int64, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params)
                Sqlgg_scope.col  ;name: (string option, 'sqlgg__brand,
                                          'sqlgg__row, 'sqlgg__params)
                                          Sqlgg_scope.col   ;.. > )
      :
      (renamed, 'sqlgg__brand, 'sqlgg__row, 'sqlgg__params) Sqlgg_scope.col=
      let open Sqlgg_scope in
        let+ id = sqlgg__cols#id
        and+ productName = sqlgg__cols#name in
        ({ id; productName } : renamed)
    let _ = renamed_of_cols
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type bad =
  | A 
  | B [@@deriving sqlgg]
include
  struct
    let _ = fun (_ : bad) -> ()
    [%%ocaml.error "deriving sqlgg: only record types are supported"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type 'a poly = {
  id: 'a }[@@deriving sqlgg]
include
  struct
    let _ = fun (_ : 'a poly) -> ()
    [%%ocaml.error "deriving sqlgg: type parameters are not supported"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
