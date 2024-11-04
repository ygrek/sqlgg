-- @create_test_30
CREATE TABLE test30 ( a INT NOT NULL, b INT NOT NULL );
-- @create_test_31
CREATE TABLE test31 ( c INT NOT NULL, d INT NOT NULL, r TEXT NOT NULL );

-- @select_all_test30
SELECT *
FROM test30
LEFT JOIN test31 on test31.c = @test31a
WHERE { c = @choice2 }? OR { r = @choice3 }! AND r = @var3 AND (c = @var4 :: Int?)
GROUP BY b;
