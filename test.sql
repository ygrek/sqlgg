
CREATE TABLE test (id INTEGER PRIMARY KEY AUTOINCREMENT,name TEXT,descr TEXT);
-- [sql2cpp] name=select_all
SELECT * FROM test;
SELECT name,descr FROM test;
-- [sql2cpp] name=Add
--INSERT INTO test VALUES;
--INSERT INTO test (name,descr) VALUES;
SELECT * FROM test WHERE name = ? LIMIT ?;
-- [sql2cpp] name=select_distinct_limit
SELECT DISTINCT * 
FROM test ORDER BY id DESC LIMIT ?;
-- [sql2cpp] name=Delete
--DELETE FROM test WHERE id = ?;

CREATE TABLE loc (id INTEGER PRIMARY KEY AUTOINCREMENT, city TEXT, test_id INTEGER);
SELECT test.id FROM test JOIN loc ON test_id = id;
SELECT test.id FROM test WHERE id = ? UNION SELECT test.id FROM test WHERE id = ?;
SELECT id+test_id AS x,? FROM loc ORDER BY id,?/test_id LIMIT ?,100;

