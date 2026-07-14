CREATE TABLE t (
    id INT,
    name TEXT
);

-- [sqlgg] dynamic_select=true
-- @in_list
SELECT id, name FROM t WHERE id IN @ids AND name = @nm;

-- [sqlgg] dynamic_select=true
-- @tuple_list
SELECT id, name FROM t WHERE (id, name) IN @pairs;

-- [sqlgg] dynamic_select=true
-- @opt_filter
SELECT id, name FROM t WHERE { id = @v }?;

-- [sqlgg] dynamic_select=true
-- @order_choice
SELECT id, name FROM t ORDER BY @sort { I { id } | N { name } };

-- [sqlgg] dynamic_select=true
-- @order_comma_choice
SELECT id, name FROM t ORDER BY id, @sort { I { id } | N { name } };

-- [sqlgg] dynamic_select=true
-- @where_choice
SELECT id, name FROM t WHERE @f { All { TRUE } | ById { id = @id } };

-- [sqlgg] dynamic_select=true
-- @shared_choice
SELECT id, name FROM t
WHERE @f { All { TRUE } | ById { id = @a } }
  AND (@f { All { TRUE } | ById { id = @b } } OR id = 0);

-- [sqlgg] dynamic_select=true
-- @kitchen_sink
SELECT id, name FROM t
WHERE id IN @ids
  AND (id, name) IN @pairs
  AND { name = @nm }?
  AND @f { All { TRUE } | ById { id = @a } }
  AND (@f { All { TRUE } | ById { id = @b } } OR id = 0)
ORDER BY @sort { I { id } | N { name } };
