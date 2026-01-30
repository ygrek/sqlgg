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

-- Test 9: Dynamic select with IN @list inside subquery branch
-- [sqlgg] dynamic_select=true
-- @with_in_subquery
SELECT id, @col { Name { name } | Filtered { (SELECT 1 FROM products WHERE price IN @prices LIMIT 1) } } FROM products WHERE id = @id;

-- Test 10: Dynamic select with arithmetic param inside branch
-- [sqlgg] dynamic_select=true
-- @with_arith_param
SELECT id, @col { AddTax { price + @tax } } FROM products WHERE id = @id;

-- Test 11: Dynamic select with two params inside branch (range)
-- [sqlgg] dynamic_select=true
-- @with_two_params
SELECT id, @col { InRange { (@min <= price) AND (price <= @max) } } FROM products WHERE id = @id;

-- Test 12: Dynamic select mixing normal param + IN @list in one branch
-- [sqlgg] dynamic_select=true
-- @with_param_and_in
SELECT id, @col { Match { CONCAT(name, @suffix) IN @names } } FROM products WHERE id = @id;

-- Test 13: Dynamic select with option-actions inside subquery WHERE
-- [sqlgg] dynamic_select=true
-- @with_option_actions_in_subquery
SELECT id, @col { Opt { (SELECT 1 FROM products WHERE { price > @min_price }? LIMIT 1) } } FROM products WHERE id = @id;

-- Test 14: Dynamic select with tuple list IN inside subquery WHERE
-- [sqlgg] dynamic_select=true
-- @with_tuple_list_in_subquery
SELECT id, @col { Pairs { (SELECT 1 FROM products WHERE (id, stock) IN @pairs LIMIT 1) } } FROM products WHERE id = @id;

-- Test 15: Dynamic select with CASE expression inside branch
-- [sqlgg] dynamic_select=true
-- @with_case_expr
SELECT id, @col { Casey { CASE WHEN id = @id2 THEN @v ELSE 0 END } } FROM products WHERE id = @id;

-- Test 16: Dynamic select with explicit type annotation inside branch
-- [sqlgg] dynamic_select=true
-- @with_typed_param
SELECT id, @col { Typed { @x :: Text } } FROM products WHERE id = @id;

-- Test 17: Monster nested scenario:
-- dynamic_select -> CASE -> Choices -> CASE -> subquery WHERE with { }? + IN @list + IN @tuple_list
-- [sqlgg] dynamic_select=true
-- @monster_nested
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

-- Test 17b: Same as Test 17 but as regular Choice (no dynamic_select)
-- This tests if the SQL generation bug is in base Choice or specific to dynamic_select
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

-- Test 18: Ultimate combo - multiple branches with different nested constructs
-- Tests: plain column, IN list, option-actions, choices, tuple list, and complex nested
-- All branches return INT to have compatible types
-- [sqlgg] dynamic_select=true
-- @ultimate_combo
SELECT id, @col {
  Plain { stock }
| WithInList { (SELECT COUNT(*) FROM products WHERE id IN @ids) }
| WithOptional { (SELECT stock FROM products WHERE { stock > @min_stock }? LIMIT 1) }
| WithChoiceAndList {
    CASE WHEN @mode { UseSum { 1 } | UseAvg { 2 } } = 1
    THEN (SELECT COUNT(*) FROM products WHERE name IN @filter_names)
    ELSE (SELECT COUNT(*) FROM products WHERE name IN @filter_names)
    END
  }
| WithTupleList { (SELECT 1 FROM products WHERE (id, stock) IN @id_stock_pairs LIMIT 1) }
| FullCombo {
    (SELECT COUNT(*)
    FROM products p2
    WHERE { p2.id = @filter_id }?
      AND p2.name IN @name_list
      AND p2.price > @min_price)
  }
} FROM products WHERE id = @id;


-- Test 19
-- [sqlgg] dynamic_select=true
-- @ultimate_combo_simple2
SELECT 
    id, 
    @col { A { name } | B { price } }, 
    category, stock, 
    @col2 { C { price + @lol { E { 2 } | F { @lol + 23 } } } | D { price } } 
FROM products;
