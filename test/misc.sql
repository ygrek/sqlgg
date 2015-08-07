SELECT strftime('%s','now');
CREATE TABLE test (x INT, `key` VARBINARY(200));
SELECT * FROM test WHERE x IS NOT NULL;
CREATE INDEX `key` ON test(`key`(20));
SELECT avg(x) FROM test;
SELECT count(*) FROM test;
