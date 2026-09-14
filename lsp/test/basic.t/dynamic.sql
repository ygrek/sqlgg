CREATE TABLE products (
  id INT PRIMARY KEY,
  name TEXT,
  price DECIMAL(10,2),
  category TEXT
);

-- [sqlgg] dynamic_select=true
-- @products
SELECT id, name, price, category FROM products WHERE id = @id;

-- [sqlgg] dynamic_select=true
SELECT price AS listed_price FROM products;

-- [sqlgg] dynamic_select=true
SELECT price AS name, name display_name, * FROM products;

-- [sqlgg] dynamic_select=true
SELECT nested_price AS outer_price
FROM (SELECT price AS nested_price FROM products) prices;
