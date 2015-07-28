SELECT strftime('%s','now');
CREATE TABLE issue20 (x INT, `key` VARBINARY(200));
SELECT * FROM issue20 WHERE x IS NOT NULL;
CREATE INDEX `key` ON issue20(`key`(20));
