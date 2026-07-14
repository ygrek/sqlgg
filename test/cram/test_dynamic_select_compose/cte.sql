CREATE TABLE t (
    id INT,
    name TEXT,
    status INT
);

-- [sqlgg] dynamic_select=true
-- @cte_plain
WITH filtered AS (SELECT id, name FROM t WHERE status = @st)
SELECT id, name FROM filtered WHERE id > @lo;

-- [sqlgg] dynamic_select=true
-- @cte_in_list
WITH filtered AS (SELECT id, name FROM t WHERE id IN @ids AND name = @nm)
SELECT id, name FROM filtered WHERE id > @lo;

-- [sqlgg] dynamic_select=true
-- @cte_choice
WITH filtered AS (SELECT id, name FROM t WHERE @f { All { TRUE } | ByStatus { status = @s } })
SELECT id, name FROM filtered ORDER BY @sort { I { id } | N { name } };

-- [sqlgg] dynamic_select=true
-- @cte_opt_filter
WITH filtered AS (SELECT id, name FROM t WHERE { status = @st }?)
SELECT id, name FROM filtered WHERE (id, name) IN @pairs;

-- [sqlgg] dynamic_select=true
-- @cte_shared_choice
WITH filtered AS (SELECT id, name FROM t WHERE @f { All { TRUE } | ById { id = @a } })
SELECT id, name FROM filtered WHERE @f { All { TRUE } | ById { id = @b } };

-- [sqlgg] dynamic_select=true
-- @cte_param_col
WITH recent AS (SELECT id, name, status FROM t WHERE status = @st)
SELECT id, name, (status >= @cutoff) AS is_new FROM recent;

-- [sqlgg] dynamic_select=true
-- @where_subquery
SELECT id, name FROM t
WHERE id IN (SELECT id FROM t WHERE status = @st AND name IN @names);
