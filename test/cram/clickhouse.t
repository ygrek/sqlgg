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

UInt64 is native ClickHouse syntax; the MySQL/TiDB UNSIGNED modifier is not.

  $ printf 'CREATE TABLE native_unsigned (id UInt64);\n' | sqlgg -no-header -dialect clickhouse -gen none -
  $ printf 'CREATE TABLE mysql_unsigned (id INT UNSIGNED);\n' | sqlgg -no-header -dialect clickhouse -gen none - 2>&1
  Feature UnsignedTypes is not supported for dialect ClickHouse (supported by: MySQL, TiDB) at INT UNSIGNED
  Errors encountered, no code generated
  [1]

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

Known ClickHouse functions reject invalid arities instead of falling back to an
untyped unknown function.

  $ printf 'SELECT has([1]);\n' | sqlgg -no-header -dialect clickhouse -gen none - 2>&1
  ==> SELECT has([1])
  Error: wrong number of arguments for ClickHouse function "has": 1
  Errors encountered, no code generated
  [1]

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

ClickHouse type names are reserved only inside a column type: elsewhere they are
ordinary column and function names.

  $ sqlgg -no-header -dialect clickhouse -gen none - <<'EOF'
  > CREATE TABLE d (date Date, uuid UUID, string String, map Map(String, Int64), array Array(Int64))
  > ENGINE = MergeTree() ORDER BY date;
  > SELECT date, uuid, string, map, array FROM d WHERE date > @since;
  > SELECT map('a', 1), tuple(1, 'x'), array(1, 2);
  > EOF

The XML generator describes container types, including element nullability and
higher-order function results.

  $ sqlgg -no-header -dialect clickhouse -gen xml - <<'EOF'
  > CREATE TABLE a (id UInt64, xs Array(Nullable(Int64)), attrs Map(String, UInt64), pair Tuple(Int64, String));
  > -- @containers
  > SELECT xs, attrs, pair, arrayMap(x -> x + @n, arrayFilter(x -> x > 0, xs)) AS shifted
  > FROM a WHERE has(xs, @v);
  > EOF
  <?xml version="1.0"?>
  
  <sqlgg>
   <stmt name="create_a" sql="CREATE TABLE a (id UInt64, xs Array(Nullable(Int64)), attrs Map(String, UInt64), pair Tuple(Int64, String))" category="DDL" kind="create" target="a" cardinality="0">
    <in/>
    <out/>
   </stmt>
   <stmt name="containers" sql="SELECT xs, attrs, pair, arrayMap(x -&gt; x + {n:Nullable(Int64)}, arrayFilter(x -&gt; x &gt; 0, xs)) AS shifted&#x0A;FROM a WHERE has(xs, {v:Nullable(Int64)})" category="DQL" kind="select" cardinality="n">
    <in>
     <value name="n" type="Int" nullable="true"/>
     <value name="v" type="Int" nullable="true"/>
    </in>
    <out>
     <value name="xs" type="Array(Int?)"/>
     <value name="attrs" type="Map(Text, UInt64)"/>
     <value name="pair" type="Tuple(Int, Text)"/>
     <value name="shifted" type="Array(Int?)"/>
    </out>
   </stmt>
   <table name="a">
    <schema>
     <value name="id" type="UInt64"/>
     <value name="xs" type="Array(Int?)"/>
     <value name="attrs" type="Map(Text, UInt64)"/>
     <value name="pair" type="Tuple(Int, Text)"/>
    </schema>
   </table>
  </sqlgg>

Generators without container codecs reject container types explicitly.

  $ sqlgg -no-header -dialect clickhouse -gen caml - 2>&1 <<'EOF' | grep -o 'type .* (no codec for containers)'
  > CREATE TABLE a (xs Array(Int64));
  > SELECT xs FROM a;
  > EOF
  type Array(Int) is not supported by the OCaml generator yet (no codec for containers)

PREWHERE filters like WHERE: its parameters come first in source order and its
IS NOT NULL checks narrow result columns. FINAL does not change result types.

  $ sqlgg -no-header -dialect clickhouse -gen xml - <<'EOF' | sed -n '/name="latest"/,/<\/stmt>/p'
  > CREATE TABLE f (id UInt64, name Nullable(String), score Int64) ENGINE = ReplacingMergeTree() ORDER BY id;
  > -- @latest
  > SELECT x.id, name FROM f AS x FINAL PREWHERE score > @min AND name IS NOT NULL WHERE id = @id;
  > EOF
   <stmt name="latest" sql="SELECT x.id, name FROM f AS x FINAL PREWHERE score &gt; {min:Int64} AND name IS NOT NULL WHERE id = {id:UInt64}" category="DQL" kind="select" cardinality="n">
    <in>
     <value name="min" type="Int"/>
     <value name="id" type="UInt64"/>
    </in>
    <out>
     <value name="id" type="UInt64"/>
     <value name="name" type="Text"/>
    </out>
   </stmt>

PREWHERE and FINAL are keywords only in ClickHouse; other dialects keep them as
identifiers.

  $ printf 'CREATE TABLE t (id INT, final INT, prewhere INT);\nSELECT final, prewhere FROM t final WHERE final.id = 1;\n' | sqlgg -no-header -dialect mysql -gen none -

Array literals and the array, tuple and map constructors are typed from their
arguments. ClickHouse containers are never NULL themselves; only their elements
can be.

  $ sqlgg -no-header -dialect clickhouse -gen xml - <<'EOF' | sed -n '/name="constructors"/,/<\/stmt>/p'
  > CREATE TABLE a (id UInt64, xs Array(Int64), name Nullable(String)) ENGINE = MergeTree() ORDER BY id;
  > -- @constructors
  > SELECT [1, 2] AS literal, [id, NULL] AS with_null, array() AS empty_array,
  >        tuple(id, name) AS pair, map(id, name) AS by_id, map() AS empty_map,
  >        has([1,
  >             2], @v) AS multiline
  > FROM a;
  > EOF
   <stmt name="constructors" sql="SELECT [1, 2] AS literal, [id, NULL] AS with_null, array() AS empty_array,&#x0A;       tuple(id, name) AS pair, map(id, name) AS by_id, map() AS empty_map,&#x0A;       has([1,&#x0A;            2], {v:Int64}) AS multiline&#x0A;FROM a" category="DQL" kind="select" cardinality="n">
    <in>
     <value name="v" type="Int"/>
    </in>
    <out>
     <value name="literal" type="Array(Int)"/>
     <value name="with_null" type="Array(UInt64?)"/>
     <value name="empty_array" type="Array(Any)"/>
     <value name="pair" type="Tuple(UInt64, Text?)"/>
     <value name="by_id" type="Map(UInt64, Text?)"/>
     <value name="empty_map" type="Map(Any, Any)"/>
     <value name="multiline" type="Bool"/>
    </out>
   </stmt>

  $ printf 'SELECT map(1, 2, 3);\n' | sqlgg -no-header -dialect clickhouse -gen none - 2>&1
  ==> SELECT map(1, 2, 3)
  Error: wrong number of arguments for ClickHouse function "map": 3
  Errors encountered, no code generated
  [1]

Square brackets stay quoted identifiers outside ClickHouse.

  $ printf 'CREATE TABLE [t] ([id] INT);\nSELECT [id] FROM [t];\n' | sqlgg -no-header -dialect sqlite -gen none -

The ClickHouse function catalog types aggregates, their combinators, parametric
aggregates, window functions and common scalar functions.

  $ cat > events.sql <<'EOF'
  > CREATE TABLE events (
  >   id Int64,
  >   user_id UInt64,
  >   name Nullable(String),
  >   at DateTime('UTC'),
  >   tags Array(String),
  >   scores Array(Nullable(Float64)),
  >   attrs Map(String, Int64),
  >   payload String
  > ) ENGINE = MergeTree ORDER BY id;
  > EOF
  $ sqlgg -no-header -dialect clickhouse -gen none events.sql -gen xml - <<'EOF' | sed '/<table /,$d' | grep -o '<value name="[a-z_]*" type="[^"]*"\( nullable="[a-z]*"\)\?'
  > SELECT uniq(user_id) AS u, uniqExact(user_id) AS ue, countIf(id > 0) AS ci, sumIf(id, id > 0) AS si,
  >        argMax(name, at) AS am, groupArray(name) AS ga, quantile(0.5)(id) AS q,
  >        quantiles(0.5, 0.9)(id) AS qs, any(name) AS an, sumArray(scores) AS sa,
  >        uniqState(user_id) AS st, toDate(at) AS d, toStartOfHour(at) AS h, toString(name) AS s,
  >        multiIf(id > 0, 'pos', id < 0, 'neg', 'zero') AS mi, toInt64OrNull(payload) AS tn,
  >        cityHash64(payload) AS hash, JSONExtractString(payload, 'k') AS js, mapKeys(attrs) AS mk
  > FROM events;
  > EOF
  <value name="u" type="UInt64"
  <value name="ue" type="UInt64"
  <value name="ci" type="Int"
  <value name="si" type="Int"
  <value name="am" type="Text" nullable="true"
  <value name="ga" type="Array(Text?)"
  <value name="q" type="Float"
  <value name="qs" type="Array(Float)"
  <value name="an" type="Text" nullable="true"
  <value name="sa" type="Float" nullable="true"
  <value name="st" type="Text"
  <value name="d" type="Datetime"
  <value name="h" type="Datetime"
  <value name="s" type="Text" nullable="true"
  <value name="mi" type="Union (neg| pos| zero)"
  <value name="tn" type="Int" nullable="true"
  <value name="hash" type="UInt64"
  <value name="js" type="Text"
  <value name="mk" type="Array(Text)"

  $ sqlgg -no-header -dialect clickhouse -gen none events.sql -gen xml - <<'EOF' | sed '/<table /,$d' | grep -o '<value name="[a-z_]*" type="[^"]*"\( nullable="[a-z]*"\)\?'
  > SELECT id, row_number() OVER (PARTITION BY user_id ORDER BY at) AS rn FROM events;
  > EOF
  <value name="id" type="Int"
  <value name="rn" type="UInt64"

Indexing reads an array element or a map value; the index is typed from the
container.

  $ sqlgg -no-header -dialect clickhouse -gen none events.sql -gen xml - <<'EOF' | sed '/<table /,$d' | grep -o '<value name="[a-z_]*" type="[^"]*"\( nullable="[a-z]*"\)\?'
  > SELECT tags[1] AS first_tag, scores[@i] AS score, attrs[@key] AS attr FROM events;
  > EOF
  <value name="i" type="Int"
  <value name="key" type="Text"
  <value name="first_tag" type="Text"
  <value name="score" type="Float" nullable="true"
  <value name="attr" type="Int"

Tuples compare element-wise, so each parameter takes the type of its column. A
subquery used as a value yields a tuple of its columns.

  $ sqlgg -no-header -dialect clickhouse -gen none events.sql -gen xml - <<'EOF' | sed '/<table /,$d' | grep -o '<value name="[a-z_]*" type="[^"]*"\( nullable="[a-z]*"\)\?'
  > SELECT (user_id, id) AS pair FROM events WHERE (user_id, id) > (@after_user, @after_id);
  > SELECT (SELECT min(id), max(id) FROM events) AS bounds;
  > EOF
  <value name="after_user" type="UInt64"
  <value name="after_id" type="Int"
  <value name="pair" type="Tuple(UInt64, Int)"
  <value name="bounds" type="Tuple(Int, Int)"

ARRAY JOIN unfolds an array into rows: an unaliased column takes the element
type, an alias adds a new column.

  $ sqlgg -no-header -dialect clickhouse -gen none events.sql -gen xml - <<'EOF' | sed '/<table /,$d' | grep -o '<value name="[a-z_]*" type="[^"]*"\( nullable="[a-z]*"\)\?\|cardinality="[^"]*"'
  > SELECT id, tags FROM events ARRAY JOIN tags;
  > SELECT id, tag, score FROM events LEFT ARRAY JOIN tags AS tag, scores AS score WHERE tag = @tag;
  > EOF
  cardinality="n"
  <value name="id" type="Int"
  <value name="tags" type="Text"
  cardinality="n"
  <value name="tag" type="Text"
  <value name="id" type="Int"
  <value name="tag" type="Text"
  <value name="score" type="Float" nullable="true"

  $ printf 'SELECT id FROM events ARRAY JOIN id;\n' | sqlgg -no-header -dialect clickhouse -gen none events.sql -gen none - 2>&1 | grep -o 'ARRAY JOIN expects an array, got Int'
  ARRAY JOIN expects an array, got Int
  $ printf 'SELECT id FROM events ARRAY JOIN mapKeys(attrs);\n' | sqlgg -no-header -dialect clickhouse -gen none events.sql -gen none - 2>&1 | grep -o 'ARRAY JOIN of an expression requires an alias'
  ARRAY JOIN of an expression requires an alias

Query clauses: LIMIT n BY, SAMPLE, SETTINGS, FORMAT, ClickHouse join strictness
and GLOBAL distribution.

  $ sqlgg -no-header -dialect clickhouse -gen none events.sql -gen xml - <<'EOF' | sed '/<table /,$d' | grep -o '<value name="[a-z_]*" type="[^"]*"'
  > SELECT id FROM events FINAL SAMPLE 1/10 OFFSET 1/2 ORDER BY at LIMIT @per_user BY user_id LIMIT @total SETTINGS max_threads = 4 FORMAT JSONEachRow;
  > SELECT e.id FROM events e ANY LEFT JOIN events f ON f.id = e.id GLOBAL INNER JOIN events g ON g.id = e.id LEFT SEMI JOIN events h ON h.id = e.id;
  > SELECT id FROM events WHERE user_id GLOBAL NOT IN (SELECT user_id FROM events WHERE id = @excluded);
  > EOF
  <value name="per_user" type="Int"
  <value name="total" type="Int"
  <value name="id" type="Int"
  <value name="id" type="Int"
  <value name="excluded" type="Int"
  <value name="id" type="Int"

ClickHouse-only syntax built from shared keywords is rejected in other dialects.

  $ printf 'CREATE TABLE t (id INT);\nSELECT id FROM t LIMIT 1 BY id;\n' | sqlgg -no-header -dialect mysql -gen none - 2>&1 | grep -o 'LIMIT BY is ClickHouse syntax'
  LIMIT BY is ClickHouse syntax
  $ printf 'CREATE TABLE t (id INT);\nSELECT a.id FROM t a ANY JOIN t b ON a.id = b.id;\n' | sqlgg -no-header -dialect mysql -gen none - 2>&1 | grep -o '[A-Z/ ]* is ClickHouse syntax'
   ANY/ALL/ASOF JOIN is ClickHouse syntax
  $ printf 'CREATE TABLE t (id INT);\nSELECT quantile(0.5)(id) FROM t;\n' | sqlgg -no-header -dialect mysql -gen none - 2>&1 | grep -o 'parametric aggregate is ClickHouse syntax'
  parametric aggregate is ClickHouse syntax

ClickHouse DDL: column codecs, TTL, materialized and alias columns, data
skipping indexes, projections, ON CLUSTER, materialized views, mutations and
OPTIMIZE.

  $ sqlgg -no-header -dialect clickhouse -gen none - <<'EOF'
  > CREATE TABLE IF NOT EXISTS logs ON CLUSTER main (
  >   id UInt64 CODEC(Delta, ZSTD(3)),
  >   at DateTime DEFAULT now() COMMENT 'event time' TTL at + INTERVAL 30 DAY,
  >   event_date Date MATERIALIZED toDate(at),
  >   event_hour DateTime ALIAS toStartOfHour(at),
  >   raw String EPHEMERAL,
  >   INDEX by_id id TYPE minmax GRANULARITY 4,
  >   PROJECTION by_date (SELECT event_date, count() GROUP BY event_date)
  > ) ENGINE = ReplacingMergeTree(id) PARTITION BY toYYYYMM(at) ORDER BY (id, at) TTL at + INTERVAL 90 DAY SETTINGS index_granularity = 8192;
  > CREATE TABLE logs_all AS logs ENGINE = Distributed(main, default, logs, rand());
  > CREATE MATERIALIZED VIEW logs_daily ENGINE = SummingMergeTree ORDER BY event_date POPULATE AS SELECT event_date, count() AS n FROM logs GROUP BY event_date;
  > ALTER TABLE logs ON CLUSTER main UPDATE at = @at WHERE id = @id;
  > ALTER TABLE logs DELETE WHERE id = @id;
  > OPTIMIZE TABLE logs ON CLUSTER main FINAL DEDUPLICATE;
  > TRUNCATE TABLE logs;
  > DROP TABLE logs_all ON CLUSTER main SYNC;
  > EOF

Types without a sqlgg representation are rejected by name.

  $ printf 'CREATE TABLE wide (x Int128);\n' | sqlgg -no-header -dialect clickhouse -gen none - 2>&1 | grep -o 'ClickHouse type [a-zA-Z0-9]* is not supported'
  ClickHouse type int128 is not supported
