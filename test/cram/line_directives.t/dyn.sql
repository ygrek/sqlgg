CREATE TABLE items (id INT NOT NULL PRIMARY KEY, name TEXT NULL);

-- [sqlgg] dynamic_select=true
-- @pick
SELECT id, name
  FROM items
 WHERE id = @id;
