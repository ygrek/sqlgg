Test Enum.to_literal generates proper SQL with quoted enum values:
  $ /bin/sh ./sqlgg_test.sh test_build_enum_literals/test_enum.sql enum_literals.compare.ml
  $ echo $?
  0

Test Enum.to_literal with actual execution (shows SQL with quotes):
  $ cd test_build_enum_literals
  $ cat test_enum.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml - > output.ml
  $ cp ../print_ocaml_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c print_ocaml_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c test_run.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -linkpkg -o test_run.exe output.ml print_ocaml_impl.ml test_run.ml
  $ ./test_run.exe
  Starting Enum Literal Tests
  ============================================================
  === Starting Enum.to_literal Tests ===
  
  [TEST 1] Testing Enum in WHERE IN clause
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT * FROM test_status WHERE status IN ('Failed', 'Skipped')
  [MOCK] Returning 1 rows
    Row 0: col0=Failed 
  [MOCK] get_column_Enum[0] = "Failed"
    -> Callback executed
  [TEST 1] Completed
  
  [TEST 2] Testing Enum in INSERT VALUES
  [MOCK EXECUTE] Connection type: [> `WR ]
  [SQL] INSERT INTO users (id, status) VALUES (1, 'Active'), (2, 'Pending')
  [MOCK] Execute result: affected_rows=2, insert_id=1
  [TEST 2] Completed: affected_rows=2
  
  === All Tests Completed Successfully ===
  
  ============================================================
  All tests executed successfully!
  $ cd ..


