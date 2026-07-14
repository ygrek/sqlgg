Shared choices (same @name used several times in one statement).

  $ cat shared.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > shared.ml
  $ diff shared.ml shared.compare.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c shared.ml

Different branches are rejected:

  $ sqlgg -gen caml -no-header - <<'EOF'
  > CREATE TABLE t (a INT, b INT);
  > SELECT * FROM t WHERE a = @x { A { 1 } | B { 2 } } AND b = @x { C { 10 } | D { 20 } };
  > EOF
  Failed : SELECT * FROM t WHERE a = @x { A { 1 } | B { 2 } } AND b = @x { C { 10 } | D { 20 } }
  At : @x { C { 10 } | D { 20 } }
  Fatal error: exception Failure("choice x is used several times with different branches")
  [2]

Different number of branches is rejected:

  $ sqlgg -gen caml -no-header - <<'EOF'
  > CREATE TABLE t (a INT, b INT);
  > SELECT * FROM t WHERE a = @x { A { 1 } | B { 2 } } AND b = @x { A { 1 } | B { 2 } | C { 3 } };
  > EOF
  Failed : SELECT * FROM t WHERE a = @x { A { 1 } | B { 2 } } AND b = @x { A { 1 } | B { 2 } | C { 3 } }
  At : @x { A { 1 } | B { 2 } | C { 3 } }
  Fatal error: exception Failure("choice x is used several times with different branches")
  [2]

Param in one occurrence, constant in the other is rejected:

  $ sqlgg -gen caml -no-header - <<'EOF'
  > CREATE TABLE t (a INT, b INT);
  > SELECT * FROM t WHERE a = @x { A { @p } | B { 2 } } AND b = @x { A { 1 } | B { 2 } };
  > EOF
  Failed : SELECT * FROM t WHERE a = @x { A { @p } | B { 2 } } AND b = @x { A { 1 } | B { 2 } }
  At : @x { A { 1 } | B { 2 } }
  Fatal error: exception Failure("choice x is used several times with different branches")
  [2]

Scalar param in one occurrence, IN-list in the other is rejected:

  $ sqlgg -gen caml -no-header - <<'EOF'
  > CREATE TABLE t (a INT, b INT);
  > SELECT * FROM t WHERE @x { P { a = @v } | N { TRUE } } AND @x { P { b IN @v } | N { TRUE } };
  > EOF
  Failed : SELECT * FROM t WHERE @x { P { a = @v } | N { TRUE } } AND @x { P { b IN @v } | N { TRUE } }
  At : @x { P { b IN @v } | N { TRUE } }
  Fatal error: exception Failure("choice x is used several times with different branches")
  [2]

Differently named params of incompatible types are rejected:

  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF'
  > CREATE TABLE t (id INT, name TEXT);
  > SELECT id FROM t WHERE @f { P { id = @a } | N { TRUE } } AND @f { P { name = @b } | N { TRUE } };
  > EOF
  Failed : SELECT id FROM t WHERE @f { P { id = @a } | N { TRUE } } AND @f { P { name = @b } | N { TRUE } }
  Fatal error: exception Failure("incompatible types for parameter \"b\" : Text and Int")
  [2]
