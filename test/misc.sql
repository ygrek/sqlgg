SELECT strftime('%s','now');
CREATE TABLE test (x INT, `key` VARBINARY(200));
SELECT * FROM test WHERE x IS NOT NULL;
CREATE INDEX `key` ON test(`key`(20));
SELECT avg(x) FROM test;
SELECT count(*) FROM test;
SELECT x FROM test WHERE ? >= `key` ORDER BY `key` DESC LIMIT 1;
SELECT x FROM test WHERE `key` < ?;

CREATE TABLE appointments (alert_at DATETIME);
INSERT INTO `appointments` (
  `alert_at`
) VALUES (
  NOW() + INTERVAL @delay SECOND
);
SELECT SUM(CASE WHEN x > 10 THEN 1 ELSE 0 END) FROM test;

CREATE TABLE issue14 (x integer);
INSERT INTO issue14 (x) VALUES (@x);
INSERT INTO issue14 SET x = @x;
INSERT INTO issue14 (x) SELECT @x;

INSERT INTO test VALUES (20, 'twenty') ON DUPLICATE KEY UPDATE x = x + ?;
INSERT INTO test VALUES (20, 'twenty') ON DUPLICATE KEY UPDATE x = VALUES(x) + ?;
INSERT INTO test VALUES (20, $$twenty$$);
INSERT INTO test VALUES (200,
$$twenty
times
ten$$);

SELECT $function$
BEGIN
    RETURN ($1 ~ $q$[\t\r\n\v\\]$q$);
END;
$function$;

INSERT INTO `test` (`x`, `key`) VALUES
(1, 'one'),
(2, 'two'),
(3, 'three');

INSERT INTO test VALUES
(1, 'one'),
(2, 'two'),
(3, 'three');

-- @issue47
SELECT count(*) > 0 FROM test;
SELECT count(*) * avg(x) FROM test;

-- @issue45
INSERT INTO test VALUES
(1, @one),
(2, @two),
(3, @one);

INSERT INTO `appointments` ( `alert_at`) VALUES (@alert);
INSERT INTO `appointments` ( `alert_at`) VALUES (FROM_UNIXTIME(@alert));

-- @count_x
SELECT COUNT(x) FROM test;

-- @count_distinct
SELECT COUNT(DISTINCT x), SUM(DISTINCT x) FROM test;

-- @issue54_mysql
SELECT 0 <=> 0, 0 <=> null, null <=> 0, null <=> null;

-- @issue54_sqlite
SELECT 0 is 0, 0 is null, null is 0, null is null;

-- @issue54_sql
SELECT 42 is not distinct from null, 42 is distinct from null;

CREATE TABLE workareas (work_id int autoincrement, about text, k int) unique key (k);
delete from test
where x in (@x1, @x2)
  and not exists (select 1 from workareas where work_id = test.x);

create table issue63 ( x text, y text );
-- @issue63
select * from issue63 where x like 'hello%' or y like 'world%';

-- @last_insert_id
INSERT INTO `workareas` (`about`,`k`) VALUES (NULLIF(@about, ''),@k)
ON DUPLICATE KEY UPDATE
  `work_id` = LAST_INSERT_ID(`work_id`);

-- @select_date
SELECT `DATE`(CURRENT_TIMESTAMP);
SELECT DATE(CURRENT_TIMESTAMP);

CREATE TABLE `adr` (
  `word` text COLLATE utf8_bin NOT NULL,
  `amount` int(10) unsigned DEFAULT '0',
  KEY `amount` (`amount`),
  KEY `word` (`word`(10))
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
