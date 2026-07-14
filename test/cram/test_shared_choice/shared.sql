CREATE TABLE t (
    id INT NOT NULL,
    name TEXT,
    status INT
);

-- @shared_const
SELECT id FROM t
WHERE id = @x { Small { 1 } | Big { 100 } }
   OR status = @x { Small { 1 } | Big { 100 } };

-- @shared_param_same_name
SELECT id FROM t
WHERE id = @x { Const { 1 } | Param { @v } }
   OR status = @x { Const { 1 } | Param { @v } };

-- @shared_params_diff_names
SELECT id FROM t
WHERE @f { All { TRUE } | ByStatus { status = @s } }
  AND @f { All { TRUE } | ByStatus { id <> @other } };

-- @shared_cte
WITH recent AS (
  SELECT id FROM t WHERE @f { All { TRUE } | ById { id = @a } }
)
SELECT id FROM recent WHERE @f { All { TRUE } | ById { id = @b } };

-- @shared_mixed
SELECT id FROM t
WHERE id IN @ids
  AND { name = @nm }?
  AND @f { All { TRUE } | ByStatus { status = @s } }
  AND (@f { All { TRUE } | ByStatus { status = @s2 } } OR id = 0)
ORDER BY @sort { I { id } | N { name } };
