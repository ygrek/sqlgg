CREATE TABLE products (
    id INT PRIMARY KEY,
    name TEXT,
    price DECIMAL(10,2),
    category TEXT,
    stock INT
);

-- Test 1: Basic dynamic select with select_one_maybe (all columns dynamic)
-- [sqlgg] dynamic_select=true
-- @select_product
SELECT id, name, price, category FROM products WHERE id = @id;

-- Test 2: Dynamic select with callback (multiple rows)
-- [sqlgg] dynamic_select=true
-- @list_products
SELECT id, name, price FROM products WHERE stock > @min_stock;

-- Test 3: Dynamic select with aliased expressions
-- [sqlgg] dynamic_select=true
-- @multi_dynamic
SELECT CONCAT(name, ' - ', category) AS label, price * stock AS total_value FROM products WHERE id = @id;

-- Test 4: Dynamic select with literal value column
-- [sqlgg] dynamic_select=true
-- @with_verbatim
SELECT id, name, 'N/A' AS fallback, category FROM products WHERE id = @id;

-- Test 5: Dynamic select with typed parameter column
-- [sqlgg] dynamic_select=true
-- @with_param
SELECT id, name, @custom_value :: Text AS custom FROM products WHERE id = @id;

-- Test 6: Dynamic select - all columns, different order
-- [sqlgg] dynamic_select=true
-- @first_position
SELECT name, price, id, stock FROM products WHERE id = @id;

-- Test 7: select_one (guaranteed single row)
-- [sqlgg] dynamic_select=true
-- @select_one_product
SELECT name, price FROM products WHERE id = @id LIMIT 1;

-- Test 8: Dynamic select with module-wrapped column
CREATE TABLE products_wrapped (
    -- [sqlgg] module=Product_id
    id INT PRIMARY KEY,
    name TEXT,
    price DECIMAL(10,2)
);

-- [sqlgg] dynamic_select=true
-- @with_module
SELECT id, name, price FROM products_wrapped WHERE id = @id;

-- Test 9: Dynamic select with subquery column (AS alias required)
-- [sqlgg] dynamic_select=true
-- @with_in_subquery
SELECT id, name, (SELECT 1 FROM products WHERE price IN @prices LIMIT 1) AS filtered FROM products WHERE id = @id;

-- Test 10: Dynamic select with arithmetic param inside expression
-- [sqlgg] dynamic_select=true
-- @with_arith_param
SELECT id, price + @tax AS add_tax FROM products WHERE id = @id;

-- Test 11: Dynamic select with two params inside expression
-- [sqlgg] dynamic_select=true
-- @with_two_params
SELECT id, (@min <= price) AND (price <= @max) AS in_range FROM products WHERE id = @id;

-- Test 12: Dynamic select mixing normal param + IN @list
-- [sqlgg] dynamic_select=true
-- @with_param_and_in
SELECT id, CONCAT(name, @suffix) IN @names AS match_ FROM products WHERE id = @id;

-- Test 13: Dynamic select with option-actions inside subquery
-- [sqlgg] dynamic_select=true
-- @with_option_actions_in_subquery
SELECT id, (SELECT 1 FROM products WHERE { price > @min_price }? LIMIT 1) AS opt FROM products WHERE id = @id;

-- Test 14: Dynamic select with tuple list IN inside subquery
-- [sqlgg] dynamic_select=true
-- @with_tuple_list_in_subquery
SELECT id, (SELECT 1 FROM products WHERE (id, stock) IN @pairs LIMIT 1) AS pairs FROM products WHERE id = @id;

-- Test 15: Dynamic select with CASE expression
-- [sqlgg] dynamic_select=true
-- @with_case_expr
SELECT id, CASE WHEN id = @id2 THEN @v ELSE 0 END AS casey FROM products WHERE id = @id;

-- Test 16: Dynamic select with explicit type annotation
-- [sqlgg] dynamic_select=true
-- @with_typed_param
SELECT id, @x :: Text AS typed FROM products WHERE id = @id;

-- Test 17: Complex subquery as plain dynamic column
-- [sqlgg] dynamic_select=true
-- @monster_nested
SELECT id,
    (SELECT
      CASE
        WHEN p2.id = @cmp_id THEN
          CASE WHEN @pick = 1 THEN @then_v ELSE @else_v END
        ELSE 0
      END
     FROM products p2
     WHERE { p2.price > @min_price }?
       AND p2.name IN @names
       AND (p2.id, p2.stock) IN @pairs
     LIMIT 1) AS monster
FROM products WHERE id = @id;

-- Test 17b: Same as Test 17 but as regular Choice (no dynamic_select)
-- @monster_nested_as_choice
SELECT id, @col {
  Monster {
    (SELECT
      CASE
        WHEN @cond { Eq { p2.id = @cmp_id } | Neq { p2.id <> @cmp_id } }
        THEN
          CASE
            WHEN (@pick { One { 1 } | Two { 2 } } = 1) THEN @then_v ELSE @else_v
          END
        ELSE 0
      END
     FROM products p2
     WHERE { p2.price > @min_price }?
       AND p2.name IN @names
       AND (p2.id, p2.stock) IN @pairs
     LIMIT 1)
  }
} FROM products WHERE id = @id;

-- Test 18: Ultimate combo - various SQL constructs as plain dynamic columns
-- [sqlgg] dynamic_select=true
-- @ultimate_combo
SELECT
    id,
    stock AS plain,
    (SELECT COUNT(*) FROM products WHERE id IN @ids) AS with_in_list,
    (SELECT stock FROM products WHERE { stock > @min_stock2 }? LIMIT 1) AS with_optional,
    CASE WHEN @mode = 1
        THEN (SELECT COUNT(*) FROM products WHERE name IN @filter_names)
        ELSE (SELECT COUNT(*) FROM products WHERE name IN @filter_names)
    END AS with_case,
    (SELECT 1 FROM products WHERE (id, stock) IN @id_stock_pairs LIMIT 1) AS with_tuple_list,
    (SELECT COUNT(*)
     FROM products p2
     WHERE { p2.id = @filter_id }?
       AND p2.name IN @name_list
       AND p2.price > @min_price) AS full_combo
FROM products WHERE id = @id;

-- Test 19: Mixed columns with arithmetic expression
-- [sqlgg] dynamic_select=true
-- @ultimate_combo_simple2
SELECT
    id,
    name,
    category, stock,
    price * (1 + @tax_rate) AS price_with_tax
FROM products;
