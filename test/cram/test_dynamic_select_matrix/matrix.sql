CREATE TABLE t (
    id INT NOT NULL,
    name TEXT NOT NULL,
    status INT NOT NULL
);

-- === position: before (construct in CTE body, dynamic column after it) ===

-- [sqlgg] dynamic_select=true
-- @before_param
WITH f AS (SELECT id, name, status FROM t WHERE status = @st)
SELECT id, name FROM f;

-- [sqlgg] dynamic_select=true
-- @before_in
WITH f AS (SELECT id, name, status FROM t WHERE id IN @ids)
SELECT id, name FROM f;

-- [sqlgg] dynamic_select=true
-- @before_not_in
WITH f AS (SELECT id, name, status FROM t WHERE id NOT IN @ids)
SELECT id, name FROM f;

-- [sqlgg] dynamic_select=true
-- @before_tuple
WITH f AS (SELECT id, name, status FROM t WHERE (id, name) IN @pairs)
SELECT id, name FROM f;

-- [sqlgg] dynamic_select=true
-- @before_opt
WITH f AS (SELECT id, name, status FROM t WHERE { status = @st }?)
SELECT id, name FROM f;

-- [sqlgg] dynamic_select=true
-- @before_choice
WITH f AS (SELECT id, name, status FROM t WHERE @c { All { TRUE } | ByS { status = @s } })
SELECT id, name FROM f;

-- === position: inside (construct inside the picked dynamic column) ===

-- [sqlgg] dynamic_select=true
-- @inside_param
SELECT id, (status >= @cutoff) AS is_new FROM t;

-- [sqlgg] dynamic_select=true
-- @inside_in
SELECT id, (id IN @ids) AS hit FROM t;

-- [sqlgg] dynamic_select=true
-- @inside_not_in
SELECT id, (id NOT IN @ids) AS miss FROM t;

-- [sqlgg] dynamic_select=true
-- @inside_tuple
SELECT id, ((id, name) IN @pairs) AS matched FROM t;

-- [sqlgg] dynamic_select=true
-- @inside_choice
SELECT id, @c { Zero { 0 } | Val { @v } } AS x FROM t;

-- === position: after (construct in main WHERE / ORDER BY, dynamic column first) ===

-- [sqlgg] dynamic_select=true
-- @after_param
SELECT id, name FROM t WHERE status = @st;

-- [sqlgg] dynamic_select=true
-- @after_in
SELECT id, name FROM t WHERE id IN @ids;

-- [sqlgg] dynamic_select=true
-- @after_not_in
SELECT id, name FROM t WHERE id NOT IN @ids;

-- [sqlgg] dynamic_select=true
-- @after_tuple
SELECT id, name FROM t WHERE (id, name) IN @pairs;

-- [sqlgg] dynamic_select=true
-- @after_opt
SELECT id, name FROM t WHERE { status = @st }?;

-- [sqlgg] dynamic_select=true
-- @after_choice
SELECT id, name FROM t WHERE @c { All { TRUE } | ByS { status = @s } };

-- === shared choice across positions ===

-- [sqlgg] dynamic_select=true
-- @shared_before_after
WITH f AS (SELECT id, name, status FROM t WHERE @c { All { TRUE } | ByS { status = @a } })
SELECT id, name FROM f WHERE @c { All { TRUE } | ByS { status = @b } };

-- === binding order across all three positions ===

-- [sqlgg] dynamic_select=true
-- @order_before_inside_after
WITH f AS (SELECT id, name, status FROM t WHERE status = @a)
SELECT id, (status >= @b) AS flag FROM f WHERE id > @c;

-- [sqlgg] dynamic_select=true
-- @mega
WITH f AS (SELECT id, name, status FROM t WHERE id IN @ids AND { status = @st }?)
SELECT id, (status >= @cutoff) AS is_new
FROM f
WHERE @c { All { TRUE } | ByS { status = @a } }
  AND (id, name) IN @pairs
  AND @c { All { TRUE } | ByS { status = @b } }
ORDER BY @sort { I { id } | N { name } };
