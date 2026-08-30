CREATE TABLE t (a INT);
-- @foo | dynamic_select true
SELECT a FROM t;
-- [sqlgg] dynamik_select=false
SELECT a FROM t;
-- [sqlgg] include=sometimes
SELECT a FROM t;
