CREATE TABLE products (
  id INT PRIMARY KEY,
  name TEXT,
  price DECIMAL(10,2),
  category TEXT
);

-- [sqlgg] dynamic_select=true
-- @products
SELECT id, name, price, category FROM products WHERE id = @id;
