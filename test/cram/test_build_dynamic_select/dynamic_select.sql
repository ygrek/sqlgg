CREATE TABLE products (
    id INT PRIMARY KEY,
    name TEXT,
    price DECIMAL(10,2),
    category TEXT,
    stock INT
);

-- Test 1: Basic dynamic select with select_one_maybe
-- [sqlgg] dynamic_select=true
-- @select_product
SELECT id, @col { Name { name } | Price { price } | Category { category } } FROM products WHERE id = @id;

-- Test 2: Dynamic select with callback (multiple rows)
-- [sqlgg] dynamic_select=true
-- @list_products
SELECT id, @col { Name { name } | Price { price } } FROM products WHERE stock > @min_stock;

-- Test 3: Multiple dynamic selects in one query
-- [sqlgg] dynamic_select=true
-- @multi_dynamic
SELECT @x { A { name } | B { category } }, @y { C { price } | D { stock } } FROM products WHERE id = @id;

-- Test 4: Dynamic select with Verbatim branches
-- [sqlgg] dynamic_select=true
-- @with_verbatim
SELECT id, @col { Name { name } | Default { 'N/A' } | Price { price } } FROM products WHERE id = @id;

-- Test 5: Dynamic select with parameter in branch
-- [sqlgg] dynamic_select=true
-- @with_param
SELECT id, @col { Static { name } | Dynamic { @custom_value :: Text } } FROM products WHERE id = @id;

-- Test 6: Dynamic select at the start of SELECT list
-- [sqlgg] dynamic_select=true
-- @first_position
SELECT @col { Name { name } | Price { price } }, id, stock FROM products WHERE id = @id;

-- Test 7: select_one (guaranteed single row)
-- [sqlgg] dynamic_select=true
-- @select_one_product
SELECT @col { Name { name } | Price { price } } FROM products WHERE id = @id LIMIT 1;

-- Test 8: Dynamic select with module-wrapped column
CREATE TABLE products_wrapped (
    -- [sqlgg] module=Product_id
    id INT PRIMARY KEY,
    name TEXT,
    price DECIMAL(10,2)
);

-- [sqlgg] dynamic_select=true
-- @with_module
SELECT @col { Id { id } | Name { name } | Price { price } } FROM products_wrapped WHERE id = @id;
