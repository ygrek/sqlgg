-- @create_table_foo
CREATE TABLE foo(
    id INTEGER PRIMARY KEY,
    foo TEXT NULL
);

-- @create_table_bar
CREATE TABLE bar(
    foo_id INTEGER NOT NULL,
    baz TEXT NOT NULL
);

-- @find
SELECT * FROM foo
WHERE id IN @ids;

-- @get
SELECT * FROM foo
WHERE (id IN @ids)
LIMIT 1;

-- @find2
SELECT * FROM foo
WHERE (id IN @ids) AND foo NOT IN @foos;

-- @find_with_bar
SELECT * FROM foo
WHERE CONCAT(foo, @suffix) IN @foos_with_suffix;

-- @get2
SELECT * FROM foo
WHERE id IN @ids AND (foo NOT IN @foos)
LIMIT 1;

-- @join
SELECT *
FROM foo f JOIN bar b ON f.id = b.foo_id
WHERE b.baz IN @bazz AND b.baz NOT IN @notbazz AND LENGTH(f.foo) IN @lengths;
