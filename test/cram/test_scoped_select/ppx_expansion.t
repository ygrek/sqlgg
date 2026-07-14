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
        (Sqlgg_scope.apply
           (Sqlgg_scope.apply
              (Sqlgg_scope.pure (fun id -> fun name -> ({ id; name } : who)))
              sqlgg__cols#id) sqlgg__cols#name : (who, 'sqlgg__brand,
                                                   'sqlgg__row, 'sqlgg__params)
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
