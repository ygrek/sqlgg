CREATE TABLE tinted_items (
    id INT PRIMARY KEY,
    -- [sqlgg] module=Color
    color ENUM('red','green','blue') NOT NULL,
    name TEXT
);

-- [sqlgg] dynamic_select=true
-- @tinted_q1
SELECT id, color, name FROM tinted_items WHERE id = @id;

-- [sqlgg] dynamic_select=true
-- @tinted_q2
SELECT name, color, id FROM tinted_items WHERE id > @min_id;
