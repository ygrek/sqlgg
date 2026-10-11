ClickHouse nested types and higher-order functions use the same recursive type
language and signature matcher as every other dialect.

  $ sqlgg -no-header -dialect clickhouse -gen none - <<'EOF'
  > CREATE TABLE events (xs Array(Int64));
  > SELECT arrayMap(x -> x + 1, xs) FROM events;
  > EOF

Nested type constructors preserve element nullability while ClickHouse columns
remain strict unless the outer type is Nullable.

  $ sqlgg -no-header -dialect clickhouse -gen none - <<'EOF'
  > CREATE TABLE typed (
  >   strict_i Int64,
  >   nullable_i Nullable(Int64),
  >   xs Array(Nullable(UInt32)),
  >   attrs Map(String, UInt64),
  >   pair Tuple(Int64, String),
  >   low LowCardinality(String),
  >   fixed_value FixedString(16),
  >   happened_at DateTime64(3, 'UTC'),
  >   amount Decimal(18, 4),
  >   compact Decimal64(2),
  >   state Enum8('ready' = 1, 'failed' = 2)
  > );
  > EOF

Higher-order signatures bind their type variables from arrays before checking
the lambda body.

  $ sqlgg -no-header -dialect clickhouse -gen none - <<'EOF'
  > CREATE TABLE events (xs Array(Int64));
  > SELECT arrayFilter(x -> x > 0, xs), arraySum(xs), has(xs, 1), length(xs)
  > FROM events;
  > EOF

  $ sqlgg -no-header -dialect clickhouse -gen none - <<'EOF'
  > CREATE TABLE paired (xs Array(Int64), ys Array(Int64));
  > SELECT arrayMap((x, y) -> x + y, xs, ys),
  >        has(arrayMap(x -> x + 1, xs), 1)
  > FROM paired;
  > EOF

ClickHouse placeholders carry the inferred parameter type.

  $ sqlgg -no-header -dialect clickhouse -params clickhouse -gen caml - <<'EOF' | grep -o '{id:Int64}' | sort -u
  > CREATE TABLE ids (id Int64);
  > -- @by_id
  > SELECT id FROM ids WHERE id = @id;
  > EOF
  {id:Int64}

A lambda result must satisfy the function template.

  $ sqlgg -no-header -dialect clickhouse -gen none - <<'EOF' 2>&1 | grep -o 'types Bool and Int do not match[^"]*'
  > CREATE TABLE events (xs Array(Int64));
  > SELECT arrayFilter(x -> x + 1, xs) FROM events;
  > EOF
  types Bool and Int do not match in ('a) -> Bool -> Array('a) -> Array('a)

Session settings parameterise the ClickHouse dialect. Outer joins pad with type
defaults unless join_use_nulls=1.

  $ cat > join.sql <<'EOF'
  > CREATE TABLE a (id Int64, x Int64);
  > CREATE TABLE b (id Int64, y Int64);
  > -- @q
  > SELECT b.y FROM a LEFT JOIN b ON a.id = b.id;
  > EOF
  $ sqlgg -no-header -dialect clickhouse -gen xml join.sql | grep -o '<value name="y"[^>]*>' | head -1
  <value name="y" type="Int"/>
  $ sqlgg -no-header -dialect clickhouse,join_use_nulls=1 -gen xml join.sql | grep -o '<value name="y"[^>]*>' | head -1
  <value name="y" type="Int" nullable="true"/>

Even with join_use_nulls=1 only scalars can become Nullable: padded arrays keep
their type and are filled with empty arrays. A padded column does not keep NOT NULL.

  $ cat > join_arrays.sql <<'EOF'
  > CREATE TABLE a (id Int64);
  > CREATE TABLE b (id Int64, y Int64, ys Array(Int64));
  > CREATE TABLE r AS SELECT b.y AS y, b.ys AS ys FROM a LEFT JOIN b ON a.id = b.id;
  > EOF
  $ sqlgg -dialect clickhouse,join_use_nulls=1 -gen none join_arrays.sql -show-table r | grep .
  r
        Int? y []
  Array(Int) ys [NotNull]

Nullable and LowCardinality wrap only scalar types.

  $ for ty in 'Nullable(Array(Int64))' 'Nullable(Nullable(Int64))' 'Nullable(LowCardinality(String))'; do
  >   printf 'CREATE TABLE z (a %s);\n' "$ty" | sqlgg -dialect clickhouse -gen none - 2>&1 | grep -c 'Tokens'
  > done
  1
  1
  1
  $ printf 'CREATE TABLE z (a LowCardinality(Nullable(String)), b Array(Nullable(Int64)));\n' > z.sql
  $ sqlgg -dialect clickhouse -gen none z.sql -show-table z | grep .
  z
       Text? a [Null]
  Array(Int?) b [NotNull]

A ClickHouse PRIMARY KEY is a sparse index, not a uniqueness constraint, so a
lookup by key may return several rows.

  $ cat > pk.sql <<'EOF'
  > CREATE TABLE p (id Int64, x Int64, PRIMARY KEY (id));
  > -- @by_id
  > SELECT x FROM p WHERE id = @id;
  > EOF
  $ sqlgg -no-header -dialect clickhouse -gen xml pk.sql | grep -o 'name="by_id"[^>]*cardinality="[^"]*"' | grep -o 'cardinality="[^"]*"'
  cardinality="n"

INSERT may omit any column: ClickHouse fills it with the type's default value.

  $ cat > insert.sql <<'EOF'
  > CREATE TABLE p (id Int64, x Int64, s String);
  > -- @ins
  > INSERT INTO p (id) VALUES (@id);
  > EOF
  $ sqlgg -no-header -dialect clickhouse -gen none insert.sql

Aggregates over an empty set return the type's default value unless
aggregate_functions_null_for_empty=1.

  $ cat > agg.sql <<'EOF'
  > CREATE TABLE g (x Int64, n Nullable(Int64));
  > -- @q
  > SELECT max(x) AS mx, sum(n) AS sn, avg(x) AS av FROM g;
  > EOF
  $ sqlgg -no-header -dialect clickhouse -gen xml agg.sql | grep -o '<value name="\(mx\|sn\|av\)"[^>]*>'
  <value name="mx" type="Int"/>
  <value name="sn" type="Int" nullable="true"/>
  <value name="av" type="Float"/>
  $ sqlgg -no-header -dialect clickhouse,aggregate_functions_null_for_empty=1 -gen xml agg.sql | grep -o '<value name="\(mx\|sn\|av\)"[^>]*>'
  <value name="mx" type="Int" nullable="true"/>
  <value name="sn" type="Int" nullable="true"/>
  <value name="av" type="Float" nullable="true"/>

Columns without NULL/NOT NULL follow data_type_default_nullable.

  $ printf 'CREATE TABLE t (a Int64, b Nullable(Int64));\n' > t.sql
  $ sqlgg -dialect clickhouse -gen none t.sql -show-table t | grep .
  t
         Int a [NotNull]
        Int? b [Null]
  $ sqlgg -dialect clickhouse,data_type_default_nullable=1 -gen none t.sql -show-table t | grep .
  t
        Int? a []
        Int? b [Null]

With transform_null_in=0 a NULL operand of IN yields false, so IN proves a value
but NOT IN does not; with transform_null_in=1 neither does.

  $ cat > in.sql <<'EOF'
  > CREATE TABLE n (v Nullable(Int64));
  > -- @in_list
  > SELECT v FROM n WHERE v IN (1, 2);
  > -- @not_in_list
  > SELECT v FROM n WHERE v NOT IN (1, 2);
  > EOF
  $ sqlgg -no-header -dialect clickhouse -gen xml in.sql | grep -o '<stmt name="[a-z_]*"\|<value name="v"[^>]*>'
  <stmt name="create_n"
  <stmt name="in_list"
  <value name="v" type="Int"/>
  <stmt name="not_in_list"
  <value name="v" type="Int" nullable="true"/>
  <value name="v" type="Int" nullable="true"/>
  $ sqlgg -no-header -dialect clickhouse,transform_null_in=1 -gen xml in.sql | grep -o '<stmt name="[a-z_]*"\|<value name="v"[^>]*>'
  <stmt name="create_n"
  <stmt name="in_list"
  <value name="v" type="Int" nullable="true"/>
  <stmt name="not_in_list"
  <value name="v" type="Int" nullable="true"/>
  <value name="v" type="Int" nullable="true"/>

Writing a nullable value into a non-nullable column needs insert_null_as_default=1.

  $ cat > write.sql <<'EOF'
  > CREATE TABLE w (a Int64, b Nullable(Int64));
  > -- @upd
  > UPDATE w SET a = b;
  > EOF
  $ sqlgg -no-header -dialect clickhouse -gen none write.sql 2>&1 | grep -o 'Cannot assign nullable value'
  Cannot assign nullable value
  $ sqlgg -no-header -dialect clickhouse,insert_null_as_default=1 -gen none write.sql

The legacy -allow-write-notnull-null flag is part of the configuration whichever
side of -dialect it is given on, and only non-strict row stores honour it.

  $ printf 'CREATE TABLE w (a INT NOT NULL, b INT);\n-- @upd\nUPDATE w SET a = b;\n' > row_write.sql
  $ sqlgg -allow-write-notnull-null -dialect mysql -gen none row_write.sql
  $ sqlgg -dialect mysql -allow-write-notnull-null -gen none row_write.sql
  $ sqlgg -allow-write-notnull-null -dialect postgresql -gen none row_write.sql 2>&1 | grep -o 'Cannot assign nullable value'
  Cannot assign nullable value

Settings are parsed at the command-line boundary.

  $ sqlgg -dialect clickhouse,bogus=1 -gen none t.sql
  unknown ClickHouse setting "bogus"
  [1]
  $ sqlgg -dialect clickhouse,join_use_nulls=2 -gen none t.sql
  invalid value "2" for ClickHouse setting join_use_nulls (expected 0 or 1)
  [1]
  $ sqlgg -dialect clickhouse,join_use_nulls -gen none t.sql
  malformed ClickHouse setting "join_use_nulls" (expected name=value)
  [1]
  $ sqlgg -dialect postgresql,join_use_nulls=1 -gen none t.sql
  dialect postgresql does not accept settings
  [1]
