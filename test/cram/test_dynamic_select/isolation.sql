CREATE TABLE t1 (
    id INT NOT NULL,
    name TEXT,
    category TEXT
);

-- [sqlgg] dynamic_select=true
-- @q1
SELECT id, name FROM t1 WHERE id = @id;

-- [sqlgg] dynamic_select=true
-- @q2
SELECT id, category FROM t1 WHERE id = @id;
