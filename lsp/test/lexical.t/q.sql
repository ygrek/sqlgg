CREATE TABLE t (a INT, b TEXT);

SELECT a FROM t WHERE b = 'oops;

SELECT a FROM t WHERE b = 'fine;';

SELECT b FROM t WHERE a = @x; /* never closed
SELECT nope FROM t;
