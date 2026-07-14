CREATE TABLE items (id INT NOT NULL PRIMARY KEY, name TEXT NULL);
CREATE TABLE stats (item_id INT NOT NULL PRIMARY KEY, sold INT NULL);

-- [sqlgg] dynamic_select=both
-- @wide
SELECT i.id, i.name, s.sold
FROM items i
LEFT JOIN stats s ON s.item_id = i.id
WHERE i.id > @min_id;
