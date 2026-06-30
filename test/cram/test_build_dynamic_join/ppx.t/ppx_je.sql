CREATE TABLE items (id INT NOT NULL PRIMARY KEY, name TEXT NULL);
CREATE TABLE details (item_id INT NOT NULL PRIMARY KEY, descr TEXT NULL);
CREATE TABLE stats (item_id INT NOT NULL PRIMARY KEY, sold INT NULL);

-- [sqlgg] dynamic_select=true
-- @wide
SELECT i.id, i.name, d.descr, s.sold
FROM items i
LEFT JOIN details d ON d.item_id = i.id
LEFT JOIN stats s ON s.item_id = i.id
WHERE i.id > @min_id;

-- [sqlgg] dynamic_select=true
-- @narrow
SELECT id, name FROM items WHERE id > @min_id;
