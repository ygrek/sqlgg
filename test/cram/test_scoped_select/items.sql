CREATE TABLE items (
    id INT PRIMARY KEY,
    -- [sqlgg] module=Color
    color TEXT NOT NULL
);

-- [sqlgg] dynamic_select=true
-- @items_scope
SELECT id, color FROM items;
