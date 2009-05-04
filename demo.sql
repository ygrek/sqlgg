CREATE TABLE test (id INTEGER PRIMARY KEY AUTOINCREMENT,name TEXT,descr TEXT);
-- [sql2cpp] name=Add
INSERT INTO test(name,descr) VALUES;
SELECT name,descr FROM test WHERE name = @name LIMIT @limit;
SELECT name,z FROM 
  (SELECT name,
          city || @delim || descr as y,
          max(length(city),random(*)) as z 
   FROM test 
   LEFT JOIN (SELECT name AS city FROM test WHERE id=@id))
WHERE z < @level;
