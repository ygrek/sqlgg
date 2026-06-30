CREATE TABLE products (
    id INT PRIMARY KEY,
    name TEXT,
    price DECIMAL(10,2),
    category TEXT,
    stock INT
);

-- [sqlgg] dynamic_select=true
-- @scope_q1
SELECT id, name, price, category FROM products WHERE id = @id;

-- [sqlgg] dynamic_select=true
-- @scope_q2
SELECT stock, id, name FROM products WHERE stock > @min_stock;
