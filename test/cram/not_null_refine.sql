CREATE TABLE items (
  id INT NOT NULL,
  name TEXT NULL,
  descr TEXT NULL,
  num INT NULL,
  num2 INT NULL
);

-- @static_not_null
SELECT name FROM items WHERE name IS NOT NULL;

-- [sqlgg] dynamic_select=true
-- @dynamic_not_null
SELECT name, descr FROM items WHERE name IS NOT NULL AND descr IS NOT NULL;

-- [sqlgg] dynamic_select=true
-- @dynamic_or_stays_nullable
SELECT name, descr FROM items WHERE name IS NOT NULL OR descr IS NOT NULL;

-- @optional_guard_stays_nullable
SELECT name FROM items WHERE {name = @name}?;

-- @in_param_is_strict
SELECT name FROM items WHERE name IN @names;

-- @not_in_param_stays_nullable
SELECT name FROM items WHERE name NOT IN @names;

-- @negated_in_param_stays_nullable
SELECT name FROM items WHERE NOT (name IN @names);

-- @in_tuple_list_is_strict
SELECT name, descr FROM items WHERE (name, descr) IN @pairs;

-- @not_in_tuple_list_stays_nullable
SELECT name, descr FROM items WHERE (name, descr) NOT IN @pairs;

-- @in_subquery_stays_nullable
SELECT name FROM items WHERE name IN (SELECT descr FROM items);

-- @between_params_are_strict
SELECT num FROM items WHERE num BETWEEN @lo AND @hi;

-- @between_bound_columns_stay_nullable
SELECT num, num2 FROM items WHERE num BETWEEN num2 AND @hi;
