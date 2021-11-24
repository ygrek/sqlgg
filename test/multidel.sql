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
SELECT * FROM foo WHERE foo = @foo;

-- @delete_foo0
DELETE foo FROM foo;

-- @delete_foo1
DELETE foo FROM foo WHERE foo = @foo;

-- @delete_foo2
DELETE FROM foo WHERE foo = @foo;

-- @delete_foo3
DELETE foo
FROM foo JOIN bar ON foo.id = bar.foo_id
WHERE bar.baz = '' OR foo.foo = @badfoo;

-- @delete_foo_and_bar
DELETE foo, bar
FROM foo JOIN bar ON foo.id = bar.foo_id
WHERE bar.baz = '' OR foo.foo = @badfoo;

-- @delete_foo_alias
DELETE f
FROM foo f JOIN bar b ON f.id = b.foo_id
WHERE b.baz = @badbaz OR f.foo = @badfoo;

-- @delete_foo_and_bar_alias
DELETE f, b
FROM foo f JOIN bar b ON f.id = b.foo_id
WHERE b.baz = @badbaz OR f.foo = @badfoo;

-- @delete_foo_and_bar_alias_rep
DELETE f, b
FROM foo f JOIN bar b ON f.id = b.foo_id
WHERE b.baz = @bad OR f.foo = @bad;
