CREATE TABLE t (id INT NOT NULL);

-- @erase
DELETE FROM t WHERE id = @id;
