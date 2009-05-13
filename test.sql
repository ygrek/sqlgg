-- [sqlgg] name=create
CREATE TABLE test (id INTEGER PRIMARY KEY AUTOINCREMENT,name TEXT,descr TEXT);
-- [sqlgg] name=select_all
SELECT * FROM test;
SELECT name,descr FROM test;
INSERT INTO test VALUES;
-- [sqlgg] name=Add
INSERT INTO test (name,descr) VALUES;
SELECT * FROM test WHERE name = @name LIMIT @limit;
-- [sqlgg] name=select_distinct_limit
SELECT DISTINCT * 
FROM test ORDER BY id DESC LIMIT ?;
-- [sqlgg] name=Delete
DELETE FROM test WHERE id = ?;
-- [sqlgg] name=Exaggerate
UPDATE test -- comment
SET descr = @extra || ' ' || descr;

CREATE TABLE loc (id INTEGER PRIMARY KEY AUTOINCREMENT, city TEXT, test_id INTEGER);
SELECT test.id FROM test JOIN loc ON test_id = test.id;
SELECT test.id FROM test WHERE id = ? UNION SELECT test.id FROM test WHERE id = ?;
SELECT id+test_id AS x,? FROM loc ORDER BY x,?/x LIMIT ?,100;

CREATE TABLE zuzu AS 
  SELECT test.id,@text || city AS city, name FROM loc JOIN test ON test_id=test.id;

SELECT x,z FROM (SELECT name as x,
  city || ' ' || descr as y,
  max(length(city),random(*)) as z FROM test LEFT JOIN (SELECT name AS city FROM test WHERE id=@id)) WHERE x > @xlo AND z < @zhi;
