What [@@deriving sqlgg] expands to:

  $ cat > who.ml <<'EOF'
  > type who = { id : int64; name : string option }
  > [@@deriving sqlgg]
  > EOF
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -dsource -c who.ml 2>&1
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
        =
        (let open Sqlgg_scope in
           let+ id = sqlgg__cols#id
           and+ name = sqlgg__cols#name in ({ id; name } : who) : (who,
                                                                    'sqlgg__brand,
                                                                    'sqlgg__row,
                                                                    'sqlgg__params)
                                                                    Sqlgg_scope.col)
      let _ = who_of_cols
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

Non-record types are rejected:

  $ cat > bad.ml <<'EOF'
  > type bad = A | B
  > [@@deriving sqlgg]
  > EOF
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -c bad.ml 2>&1
  File "bad.ml", lines 1-2, characters 0-18:
  1 | type bad = A | B
  2 | [@@deriving sqlgg]
  Error: deriving sqlgg: only record types are supported
  [2]

[@sqlgg.col] maps a record field to a differently named column:

  $ cat > renamed.ml <<'XEOF'
  > type renamed = { id : int64; productName : string option [@sqlgg.col "name"] }
  > [@@deriving sqlgg]
  > XEOF
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -dsource -c renamed.ml 2>&1
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
        =
        (let open Sqlgg_scope in
           let+ id = sqlgg__cols#id
           and+ productName = sqlgg__cols#name in
           ({ id; productName } : renamed) : (renamed, 'sqlgg__brand,
                                               'sqlgg__row, 'sqlgg__params)
                                               Sqlgg_scope.col)
      let _ = renamed_of_cols
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

Type parameters are rejected:

  $ cat > poly.ml <<'XEOF'
  > type 'a poly = { id : 'a }
  > [@@deriving sqlgg]
  > XEOF
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -c poly.ml 2>&1
  File "poly.ml", lines 1-2, characters 0-18:
  1 | type 'a poly = { id : 'a }
  2 | [@@deriving sqlgg]
  Error: deriving sqlgg: type parameters are not supported
  [2]
  $ echo done
  done
