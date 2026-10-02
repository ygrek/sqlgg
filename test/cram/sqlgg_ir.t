Schema-less stdin: each statement carries its SQL, props and a resolution
(Parsed, Analyzed or Invalid) with the full AST inside.

  $ sqlgg ir <<'SQL'
  > SELECT 1;
  > SQL
  {"statements":[{"sql":"SELECT 1","props":[],"resolution":["Parsed",{"stmt":["Select",{"select_complete":{"select":[{"source_pos":[0,8],"columns":[{"value":["Expr",{"value":["Value",{"collated":{"t":"Int","nullability":"Strict"},"collation":null}],"pos":[7,8]},null],"pos":[7,8]}],"from":null,"where":null,"group":[],"having":null},[]],"order":[],"limit":null,"select_row_locking":null},"cte":null}],"dialect_features":[]}]}]}

WHERE, GROUP BY, HAVING, ORDER BY and LIMIT survive in the parsed AST:

  $ sqlgg ir - <<'SQL'
  > SELECT id, COUNT(*) FROM users WHERE id > @min AND name LIKE 'a%' GROUP BY id HAVING COUNT(*) > 1 ORDER BY id DESC LIMIT @lim;
  > SQL
  {"statements":[{"sql":"SELECT id, COUNT(*) FROM users WHERE id > @min AND name LIKE 'a%' GROUP BY id HAVING COUNT(*) > 1 ORDER BY id DESC LIMIT @lim","props":[],"resolution":["Parsed",{"stmt":["Select",{"select_complete":{"select":[{"source_pos":[0,125],"columns":[{"value":["Expr",{"value":["Column",{"collated":{"cname":"id","tname":null,"cpos":[7,9]},"collation":null}],"pos":[7,9]},null],"pos":[7,9]},{"value":["Expr",{"value":["Fun",{"fn_name":"count","kind":["Agg","Count"],"parameters":[],"over":null,"fn_pos":[11,19]}],"pos":[11,19]},null],"pos":[11,19]}],"from":[[["Table",{"db":null,"tn":"users"}],null],[]],"where":["Fun",{"fn_name":"boolean_bin_op","kind":["Logical","And"],"parameters":[["Fun",{"fn_name":"comparison","kind":["Comparison","Comp_num_cmp"],"parameters":[["Column",{"collated":{"cname":"id","tname":null,"cpos":[37,39]},"collation":null}],["Param",{"id":{"value":"min","pos":[42,46]},"typ":{"t":["Infer","Any"],"nullability":"Depends"}},{}]],"over":null,"fn_pos":[37,46]}],["Fun",{"fn_name":"like","kind":"Like","parameters":[["Column",{"collated":{"cname":"name","tname":null,"cpos":[51,55]},"collation":null}],["Value",{"collated":{"t":["StringLiteral","a%"],"nullability":"Strict"},"collation":null}]],"over":null,"fn_pos":[51,65]}]],"over":null,"fn_pos":[37,65]}],"group":[["Column",{"collated":{"cname":"id","tname":null,"cpos":[75,77]},"collation":null}]],"having":["Fun",{"fn_name":"comparison","kind":["Comparison","Comp_num_cmp"],"parameters":[["Fun",{"fn_name":"count","kind":["Agg","Count"],"parameters":[],"over":null,"fn_pos":[85,93]}],["Value",{"collated":{"t":"Int","nullability":"Strict"},"collation":null}]],"over":null,"fn_pos":[85,97]}]},[]],"order":[[["Column",{"collated":{"cname":"id","tname":null,"cpos":[107,109]},"collation":null}],"Fixed"]],"limit":[[{"id":{"value":"lim","pos":[121,125]},"typ":{"t":["Infer","Int"],"nullability":"Strict"}}],false],"select_row_locking":null},"cte":null}],"dialect_features":[]}]}]}

Schema-aware input is Analyzed: the same full AST plus the typed statement.

  $ cat > schema.sql <<'SQL'
  > CREATE TABLE users (id INT NOT NULL, name TEXT);
  > SQL
  $ sqlgg ir -schema schema.sql - <<'SQL'
  > SELECT id FROM users WHERE id = @id;
  > SQL
  {"statements":[{"sql":"SELECT id FROM users WHERE id = @id","props":[],"resolution":["Analyzed",{"parsed":{"stmt":["Select",{"select_complete":{"select":[{"source_pos":[0,35],"columns":[{"value":["Expr",{"value":["Column",{"collated":{"cname":"id","tname":null,"cpos":[7,9]},"collation":null}],"pos":[7,9]},null],"pos":[7,9]}],"from":[[["Table",{"db":null,"tn":"users"}],null],[]],"where":["Fun",{"fn_name":"comparison","kind":["Comparison","Comp_equal"],"parameters":[["Column",{"collated":{"cname":"id","tname":null,"cpos":[27,29]},"collation":null}],["Param",{"id":{"value":"id","pos":[32,35]},"typ":{"t":["Infer","Any"],"nullability":"Depends"}},{}]],"over":null,"fn_pos":[27,35]}],"group":[],"having":null},[]],"order":[],"limit":null,"select_row_locking":null},"cte":null}],"dialect_features":[]},"typed":{"sql":"SELECT id FROM users WHERE id = @id","schema":[["Attr",{"name":"id","domain":{"t":"Int","nullability":"Strict"},"extra":["NotNull"],"meta":{}}]],"vars":[["Single",{"id":{"value":"id","pos":[32,35]},"typ":{"t":"Int","nullability":"Strict"}},{}]],"kind":["Select","Nat"]}}]}]}

Column annotations reach the AST, with and without a schema:

  $ cat > annotated.sql <<'SQL'
  > CREATE TABLE annotated (
  >   id INT NOT NULL,
  >   -- [sqlgg] module=Codecs.Cid
  >   cid BIGINT NOT NULL
  > );
  > SQL
  $ sqlgg ir annotated.sql
  {"statements":[{"sql":"CREATE TABLE annotated (\n  id INT NOT NULL,\n  -- [sqlgg] module=Codecs.Cid\n  cid BIGINT NOT NULL\n)","props":[],"resolution":["Parsed",{"stmt":["Create",{"value":{"db":null,"tn":"annotated"},"pos":[13,22]},["Schema",{"schema":[{"name":{"value":"id","pos":[27,29]},"kind":{"value":{"collated":["Int",{"size":null,"sign":"Signed","display_width":null}],"collation":null},"pos":[30,33]},"extra":[{"value":["Syntax_constraint","NotNull"],"pos":[34,42]}],"meta":[]},{"name":{"value":"cid","pos":[77,80]},"kind":{"value":{"collated":["Int",{"size":"Big","sign":"Signed","display_width":null}],"collation":null},"pos":[81,87]},"extra":[{"value":["Syntax_constraint","NotNull"],"pos":[88,96]}],"meta":[["module","Codecs.Cid"]]}],"constraints":[],"indexes":[]}]],"dialect_features":[]}]}]}
  $ sqlgg ir -schema schema.sql annotated.sql
  {"statements":[{"sql":"CREATE TABLE annotated (\n  id INT NOT NULL,\n  -- [sqlgg] module=Codecs.Cid\n  cid BIGINT NOT NULL\n)","props":[],"resolution":["Analyzed",{"parsed":{"stmt":["Create",{"value":{"db":null,"tn":"annotated"},"pos":[13,22]},["Schema",{"schema":[{"name":{"value":"id","pos":[27,29]},"kind":{"value":{"collated":["Int",{"size":null,"sign":"Signed","display_width":null}],"collation":null},"pos":[30,33]},"extra":[{"value":["Syntax_constraint","NotNull"],"pos":[34,42]}],"meta":[]},{"name":{"value":"cid","pos":[77,80]},"kind":{"value":{"collated":["Int",{"size":"Big","sign":"Signed","display_width":null}],"collation":null},"pos":[81,87]},"extra":[{"value":["Syntax_constraint","NotNull"],"pos":[88,96]}],"meta":[["module","Codecs.Cid"]]}],"constraints":[],"indexes":[]}]],"dialect_features":[]},"typed":{"sql":"CREATE TABLE annotated (\n  id INT NOT NULL,\n  -- [sqlgg] module=Codecs.Cid\n  cid BIGINT NOT NULL\n)","schema":[],"vars":[],"kind":["Create",{"db":null,"tn":"annotated"}]}}]}]}

Statement properties are kept, with and without a schema:

  $ cat > props.sql <<'SQL'
  > -- @get_user
  > SELECT id FROM users WHERE id = @id;
  > SQL
  $ sqlgg ir props.sql
  {"statements":[{"sql":"SELECT id FROM users WHERE id = @id","props":[["Name","get_user"]],"resolution":["Parsed",{"stmt":["Select",{"select_complete":{"select":[{"source_pos":[0,35],"columns":[{"value":["Expr",{"value":["Column",{"collated":{"cname":"id","tname":null,"cpos":[7,9]},"collation":null}],"pos":[7,9]},null],"pos":[7,9]}],"from":[[["Table",{"db":null,"tn":"users"}],null],[]],"where":["Fun",{"fn_name":"comparison","kind":["Comparison","Comp_equal"],"parameters":[["Column",{"collated":{"cname":"id","tname":null,"cpos":[27,29]},"collation":null}],["Param",{"id":{"value":"id","pos":[32,35]},"typ":{"t":["Infer","Any"],"nullability":"Depends"}},{}]],"over":null,"fn_pos":[27,35]}],"group":[],"having":null},[]],"order":[],"limit":null,"select_row_locking":null},"cte":null}],"dialect_features":[]}]}]}
  $ sqlgg ir -schema schema.sql props.sql
  {"statements":[{"sql":"SELECT id FROM users WHERE id = @id","props":[["Name","get_user"]],"resolution":["Analyzed",{"parsed":{"stmt":["Select",{"select_complete":{"select":[{"source_pos":[0,35],"columns":[{"value":["Expr",{"value":["Column",{"collated":{"cname":"id","tname":null,"cpos":[7,9]},"collation":null}],"pos":[7,9]},null],"pos":[7,9]}],"from":[[["Table",{"db":null,"tn":"users"}],null],[]],"where":["Fun",{"fn_name":"comparison","kind":["Comparison","Comp_equal"],"parameters":[["Column",{"collated":{"cname":"id","tname":null,"cpos":[27,29]},"collation":null}],["Param",{"id":{"value":"id","pos":[32,35]},"typ":{"t":["Infer","Any"],"nullability":"Depends"}},{}]],"over":null,"fn_pos":[27,35]}],"group":[],"having":null},[]],"order":[],"limit":null,"select_row_locking":null},"cte":null}],"dialect_features":[]},"typed":{"sql":"SELECT id FROM users WHERE id = @id","schema":[["Attr",{"name":"id","domain":{"t":"Int","nullability":"Strict"},"extra":["NotNull"],"meta":{}}]],"vars":[["Single",{"id":{"value":"id","pos":[32,35]},"typ":{"t":"Int","nullability":"Strict"}},{}]],"kind":["Select","Nat"]}}]}]}

Inputs never mutate the loaded schema:

  $ sqlgg ir -schema schema.sql - <<'SQL'
  > CREATE TABLE extra (x INT);
  > SELECT x FROM extra;
  > SQL
  {"statements":[{"sql":"CREATE TABLE extra (x INT)","props":[],"resolution":["Analyzed",{"parsed":{"stmt":["Create",{"value":{"db":null,"tn":"extra"},"pos":[13,18]},["Schema",{"schema":[{"name":{"value":"x","pos":[20,21]},"kind":{"value":{"collated":["Int",{"size":null,"sign":"Signed","display_width":null}],"collation":null},"pos":[22,25]},"extra":[],"meta":[]}],"constraints":[],"indexes":[]}]],"dialect_features":[]},"typed":{"sql":"CREATE TABLE extra (x INT)","schema":[],"vars":[],"kind":["Create",{"db":null,"tn":"extra"}]}}]},{"sql":"SELECT x FROM extra","props":[],"resolution":["Invalid",{"diagnostics":[{"message":"no such table extra","location":"Statement"}]}]}]}
  [1]

Malformed SQL yields Invalid, exit 1, nothing on stderr:

  $ sqlgg ir - <<'SQL' 2>err
  > SELECT FROM;
  > SQL
  {"statements":[{"sql":"SELECT FROM","props":[],"resolution":["Invalid",{"diagnostics":[{"message":"syntax error","location":["Span",[7,11]]}]}]}]}
  [1]
  $ cat err

A property error in the comment before a statement has no position inside its sql:

  $ sqlgg ir - <<'SQL'
  > -- [sqlgg] bogus=1
  > SELECT 1;
  > SQL
  {"statements":[{"sql":"SELECT 1","props":[],"resolution":["Invalid",{"diagnostics":[{"message":"unknown property bogus","location":"Properties"}]}]}]}
  [1]

Mixed valid and invalid statements keep input order:

  $ sqlgg ir - <<'SQL'
  > SELECT 1;
  > SELECT FROM;
  > SELECT 2;
  > SQL
  {"statements":[{"sql":"SELECT 1","props":[],"resolution":["Parsed",{"stmt":["Select",{"select_complete":{"select":[{"source_pos":[0,8],"columns":[{"value":["Expr",{"value":["Value",{"collated":{"t":"Int","nullability":"Strict"},"collation":null}],"pos":[7,8]},null],"pos":[7,8]}],"from":null,"where":null,"group":[],"having":null},[]],"order":[],"limit":null,"select_row_locking":null},"cte":null}],"dialect_features":[]}]},{"sql":"SELECT FROM","props":[],"resolution":["Invalid",{"diagnostics":[{"message":"syntax error","location":["Span",[7,11]]}]}]},{"sql":"SELECT 2","props":[],"resolution":["Parsed",{"stmt":["Select",{"select_complete":{"select":[{"source_pos":[0,8],"columns":[{"value":["Expr",{"value":["Value",{"collated":{"t":"Int","nullability":"Strict"},"collation":null}],"pos":[7,8]},null],"pos":[7,8]}],"from":null,"where":null,"group":[],"having":null},[]],"order":[],"limit":null,"select_row_locking":null},"cte":null}],"dialect_features":[]}]}]}
  [1]

Multiple files are processed in argument order into one compact document:

  $ printf 'SELECT 1;\nSELECT 2;\n' > a.sql
  $ printf 'SELECT 3;\n' > b.sql
  $ sqlgg ir a.sql b.sql
  {"statements":[{"sql":"SELECT 1","props":[],"resolution":["Parsed",{"stmt":["Select",{"select_complete":{"select":[{"source_pos":[0,8],"columns":[{"value":["Expr",{"value":["Value",{"collated":{"t":"Int","nullability":"Strict"},"collation":null}],"pos":[7,8]},null],"pos":[7,8]}],"from":null,"where":null,"group":[],"having":null},[]],"order":[],"limit":null,"select_row_locking":null},"cte":null}],"dialect_features":[]}]},{"sql":"SELECT 2","props":[],"resolution":["Parsed",{"stmt":["Select",{"select_complete":{"select":[{"source_pos":[0,8],"columns":[{"value":["Expr",{"value":["Value",{"collated":{"t":"Int","nullability":"Strict"},"collation":null}],"pos":[7,8]},null],"pos":[7,8]}],"from":null,"where":null,"group":[],"having":null},[]],"order":[],"limit":null,"select_row_locking":null},"cte":null}],"dialect_features":[]}]},{"sql":"SELECT 3","props":[],"resolution":["Parsed",{"stmt":["Select",{"select_complete":{"select":[{"source_pos":[0,8],"columns":[{"value":["Expr",{"value":["Value",{"collated":{"t":"Int","nullability":"Strict"},"collation":null}],"pos":[7,8]},null],"pos":[7,8]}],"from":null,"where":null,"group":[],"having":null},[]],"order":[],"limit":null,"select_row_locking":null},"cte":null}],"dialect_features":[]}]}]}

--plain-sql rejects sqlgg extensions and accepts plain SQL:

  $ sqlgg ir -plain-sql - <<'SQL'
  > SELECT * FROM t WHERE id = @id;
  > SELECT id FROM t WHERE id = 1;
  > SQL
  {"statements":[{"sql":"SELECT * FROM t WHERE id = @id","props":[],"resolution":["Invalid",{"diagnostics":[{"message":"sqlgg extension not allowed: query parameter (@name / ?)","location":["Span",[27,30]]}]}]},{"sql":"SELECT id FROM t WHERE id = 1","props":[],"resolution":["Parsed",{"stmt":["Select",{"select_complete":{"select":[{"source_pos":[0,29],"columns":[{"value":["Expr",{"value":["Column",{"collated":{"cname":"id","tname":null,"cpos":[7,9]},"collation":null}],"pos":[7,9]},null],"pos":[7,9]}],"from":[[["Table",{"db":null,"tn":"t"}],null],[]],"where":["Fun",{"fn_name":"comparison","kind":["Comparison","Comp_equal"],"parameters":[["Column",{"collated":{"cname":"id","tname":null,"cpos":[23,25]},"collation":null}],["Value",{"collated":{"t":"Int","nullability":"Strict"},"collation":null}]],"over":null,"fn_pos":[23,29]}],"group":[],"having":null},[]],"order":[],"limit":null,"select_row_locking":null},"cte":null}],"dialect_features":[]}]}]}
  [1]

--json-schema prints the generated Draft 2020-12 schema:

  $ sqlgg ir -json-schema > schema.json
  $ grep -F '"$schema": "https://json-schema.org/draft/2020-12/schema"' schema.json
    "$schema": "https://json-schema.org/draft/2020-12/schema",
  $ grep -oE '"const": "(Parsed|Analyzed|Invalid)"' schema.json | sort -u
  "const": "Analyzed"
  "const": "Invalid"
  "const": "Parsed"

Missing input file or bad arguments are CLI errors (exit 2), not Invalid:

  $ sqlgg ir missing.sql
  cannot read missing.sql: missing.sql: No such file or directory
  [2]
  $ sqlgg ir -schema 2>/dev/null
  [2]
  $ sqlgg ir --bogus 2>/dev/null
  [2]
  $ sqlgg ir -schema missing.sql a.sql
  cannot read missing.sql: missing.sql: No such file or directory
  [2]
