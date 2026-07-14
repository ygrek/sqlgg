CREATE TABLE test(a INT, b INT);

SELECT * FROM test
WHERE a = @x { A { 1 } | B { 2 } }
  AND b = @x { A { 10 } | B { 20 } };

SELECT * FROM test
WHERE a = @y { Const { 1 } | Param { @v } }
  AND b = @y { Const { 1 } | Param { @v } };

SELECT * FROM test
WHERE a = @z { Const { 1 } | Param { @p } }
  AND b = @z { Const { 1 } | Param { @q } };
