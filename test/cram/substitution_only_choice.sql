CREATE TABLE subst_items (
  id INT NOT NULL,
  group_id INT NOT NULL DEFAULT 0,
  score INT NOT NULL DEFAULT 0
);

-- @list_first
SELECT 1 WHERE @f { A { 1 IN @ids AND 1 = @n } };

-- @list_last
SELECT 1 WHERE @f { A { 1 = @n AND 1 IN @ids } };

-- @multiple_lists
SELECT 1 WHERE @f { A { 1 IN @ids AND 1 = @n AND 2 IN @more_ids } };

-- @tuple_list
SELECT 1 WHERE @f { A { (1, 2) IN @pairs AND 1 = @n } };

-- @nested
SELECT 1 WHERE @outer {
  A { @inner { B { 1 IN @ids AND 1 = @n } | C { 1 = @m } } }
  | D { TRUE }
};

-- @repeated_list
SELECT 1 WHERE @repeated {
  A { 1 IN @repeated_ids OR 2 IN @repeated_ids OR 1 = @repeated_n }
};

-- @duplicate_bound
SELECT id
FROM subst_items
WHERE @duplicate {
  A { id = @same AND score = @same AND group_id IN @duplicate_ids }
};

-- @legacy_bound_order
SELECT 1
WHERE @search {
  A { @query = 1 OR @email_query = 2 OR @query = 3 }
};

-- @projection
SELECT @projection_choice {
  A { id IN @projection_ids AND score = @projection_n }
  | B { TRUE }
}
FROM subst_items;

-- @join_on
SELECT a.id
FROM subst_items a
JOIN subst_items b ON @join_choice {
  A { b.id IN @join_ids AND a.id = @join_n }
  | B { TRUE }
};

-- @having
SELECT group_id, COUNT(*)
FROM subst_items
GROUP BY group_id
HAVING @having_choice {
  A { group_id IN @having_ids AND COUNT(*) > @having_n }
  | B { TRUE }
};

-- @order_by
SELECT id
FROM subst_items
ORDER BY @order_choice {
  A { CASE WHEN id IN @order_ids THEN @order_n ELSE id END }
  | B { id }
};

-- @subquery
SELECT (
  SELECT @subquery_choice {
    A { 1 IN @subquery_ids AND 1 = @subquery_n }
    | B { TRUE }
  }
);

-- @optional
SELECT id
FROM subst_items
WHERE { id IN @optional_ids AND score = @optional_n }?;

-- @update_set
UPDATE subst_items
SET score = @update_choice {
  A { CASE WHEN id IN @update_ids THEN @update_n ELSE score END }
  | B { score }
};

-- @delete_where
DELETE FROM subst_items
WHERE @delete_choice {
  A { id IN @delete_ids AND score = @delete_n }
  | B { TRUE }
};

-- @insert_select
INSERT INTO subst_items (id)
SELECT @insert_choice {
  A { CASE WHEN 1 IN @insert_ids THEN @insert_n ELSE 0 END }
  | B { 0 }
};

-- @function_arg
SELECT COALESCE(@function_choice {
  A { CASE WHEN id IN @function_ids THEN @function_n ELSE 0 END }
  | B { 0 }
}, 0)
FROM subst_items;

-- @aggregate_arg
SELECT SUM(@aggregate_choice {
  A { CASE WHEN id IN @aggregate_ids THEN @aggregate_n ELSE 0 END }
  | B { 0 }
})
FROM subst_items;

-- @flat_case_control
SELECT CASE WHEN id IN @case_ids THEN @case_n ELSE score END
FROM subst_items;

-- @optional_nested_choice
SELECT id
FROM subst_items
WHERE { @optional_pick {
  A { id IN @optional_nested_ids AND score = @optional_nested_n }
  | B { TRUE }
} }?;

-- @choice_nested_optional
SELECT id
FROM subst_items
WHERE @nested_optional_pick {
  A { { id IN @nested_optional_ids AND score = @nested_optional_n }? AND group_id = @nested_optional_g }
  | B { TRUE }
};

-- @repeated_tuple_list
SELECT id
FROM subst_items
WHERE @repeated_tuple_pick {
  A {
    (id, group_id) IN @repeated_pairs
    OR (score, group_id) IN @repeated_pairs
    OR id = @repeated_tuple_n
  }
  | B { TRUE }
};

-- @case_choice_condition
SELECT CASE
  WHEN @case_choice {
    A { id IN @case_choice_ids AND score = @case_choice_n }
    | B { TRUE }
  }
  THEN 1
  ELSE 0
END
FROM subst_items;

-- @set_default_nested_choice
UPDATE subst_items
SET score = { @default_pick {
  A { CASE WHEN id IN @default_ids THEN @default_n ELSE score END }
  | B { score }
} }??;

-- @shared_filter | include: reuse
SELECT id
FROM subst_items
WHERE id IN @shared_ids AND score = @shared_n;

-- @shared_inside_choice
SELECT @shared_choice {
  A { EXISTS (WITH filtered AS &shared_filter SELECT id FROM filtered) }
  | B { TRUE }
};
