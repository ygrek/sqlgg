CREATE TABLE t (
    id INT NOT NULL,
    name TEXT NOT NULL,
    status INT NOT NULL
);

-- @frag_param | include: reuse
SELECT id, name, status FROM t WHERE status = @st;

-- @frag_mixed | include: reuse
SELECT id, name, status FROM t
WHERE id IN @ids AND { status = @st }? AND @c { All { TRUE } | ByS { status = @s } };

-- [sqlgg] dynamic_select=true
-- @reuse_param
WITH f AS &frag_param
SELECT id, name FROM f WHERE id > @lo;

-- [sqlgg] dynamic_select=true
-- @reuse_param_col
WITH f AS &frag_param
SELECT id, (status >= @cutoff) AS is_new FROM f;

-- [sqlgg] dynamic_select=true
-- @reuse_mixed
WITH f AS &frag_mixed
SELECT id, name
FROM f
WHERE (id, name) IN @pairs
ORDER BY @sort { I { id } | N { name } };

-- [sqlgg] dynamic_select=true
-- @reuse_twice
WITH a AS &frag_param, b AS &frag_param
SELECT a.id, a.name FROM a JOIN b ON a.id = b.id WHERE a.id > @lo;
