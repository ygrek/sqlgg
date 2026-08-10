CREATE TABLE accounts (
  id BIGINT NOT NULL,
  -- [sqlgg] module=Codecs.Cid
  cid BIGINT NOT NULL,
  -- [sqlgg] module=Codecs.Status
  status TEXT NOT NULL,
  plain TEXT NOT NULL
);
CREATE TABLE orders (
  -- [sqlgg] module=Codecs.Order_id
  id BIGINT NOT NULL,
  -- [sqlgg] module=Codecs.Money
  amount DECIMAL(10,2) NOT NULL
);
CREATE TABLE events (
  order_ref BIGINT NOT NULL,
  amount BIGINT NOT NULL
);

CREATE TABLE named_owners (
  -- [sqlgg] module=Codecs.Owner_id
  id BIGINT NOT NULL,
  title TEXT NOT NULL
);

CREATE TABLE named_owned (
  id BIGINT NOT NULL,
  note TEXT NOT NULL
);

CREATE TABLE other_domain (
  -- [sqlgg] module=Codecs.Course_id
  id BIGINT NOT NULL
);

CREATE TABLE owners (
  -- [sqlgg] module=Codecs.Owner_id
  id BIGINT NOT NULL PRIMARY KEY
);
CREATE TABLE owned (
  owner_ref BIGINT NOT NULL,
  loose BIGINT NOT NULL,
  FOREIGN KEY (owner_ref) REFERENCES owners(id)
);
CREATE TABLE unrelated (
  owner_ref BIGINT NOT NULL
);

CREATE TABLE projects (
  id BIGINT NOT NULL,
  -- [sqlgg] module=Codecs.Company_id
  company_id BIGINT NOT NULL
);
CREATE TABLE alerts (
  dashboard_id BIGINT NOT NULL,
  company_id BIGINT NOT NULL
);
CREATE TABLE courses (
  -- [sqlgg] module=Codecs.Slug
  slug TEXT NOT NULL,
  -- [sqlgg] module=Codecs.Db_int
  seconds BIGINT NOT NULL
);

CREATE TABLE left_rows (
  -- [sqlgg] module=Codecs.Left_id
  id BIGINT NOT NULL
);
CREATE TABLE right_rows (
  -- [sqlgg] module=Codecs.Right_id
  id BIGINT NOT NULL,
  -- [sqlgg] module=Codecs.Row_status
  status ENUM('draft','published','failed') NOT NULL
);

CREATE TABLE codec_rows (
  id INT NOT NULL,
  -- [sqlgg] get_column=Codecs.Order_status.of_db
  -- [sqlgg] set_param=Codecs.Order_status.to_db
  status ENUM('new','paid','shipped') NOT NULL
);

-- @shared_by_equality
SELECT events.order_ref, orders.id FROM orders JOIN events ON orders.id = events.order_ref;
-- @disjunction_withdraws_it
SELECT events.order_ref FROM orders JOIN events ON orders.id = events.order_ref OR events.order_ref = 0;
-- @representation_mismatch
SELECT events.amount FROM orders JOIN events ON orders.amount = events.amount;

-- @towards_the_nullable_side
SELECT events.order_ref FROM orders LEFT JOIN events ON orders.id = events.order_ref;
-- @towards_the_preserved_side
SELECT events.order_ref FROM events LEFT JOIN orders ON orders.id = events.order_ref;

-- @no_join
SELECT owner_ref, loose FROM owned;
-- @outer_join_preserved_side
SELECT owned.owner_ref FROM owned LEFT JOIN owners ON owners.id = owned.owner_ref;
-- @without_the_declaration
SELECT unrelated.owner_ref FROM unrelated LEFT JOIN owners ON owners.id = unrelated.owner_ref;

-- @through_a_null_handling_call
SELECT id FROM accounts WHERE IFNULL(@a, status) = @b;
-- @through_nested_coalesce
SELECT COALESCE(COALESCE(cid, @a), @b) AS c FROM accounts;
-- @through_nullif
SELECT NULLIF(status, @param) AS s FROM accounts;

-- @greatest_over_a_literal
SELECT GREATEST(cid, 0) AS c FROM accounts;
-- @least_feeds_the_param
SELECT id FROM accounts WHERE cid = LEAST(@param, 0);

-- @untyped_sibling
SELECT COALESCE(projects.company_id, alerts.company_id) AS company_id
FROM alerts LEFT JOIN projects ON alerts.dashboard_id = projects.id;
-- @literal_fallback
SELECT COALESCE(slug, '') AS module_slug FROM courses;
-- @selecting_aggregate_keeps_it
SELECT COALESCE(MAX(seconds), 0) AS longest FROM courses;
-- @computing_aggregate_keeps_it
SELECT COALESCE(SUM(seconds), 0) AS total FROM courses;

-- @transforming_function
SELECT id FROM accounts WHERE LOWER(status) = @param;
-- @arithmetic
SELECT id FROM accounts WHERE cid = @param + 1;
-- @concatenation
SELECT CONCAT(status, plain) AS s FROM accounts;

-- @case_literal_fallback
SELECT CASE WHEN id = 1 THEN status ELSE 'active' END AS s FROM accounts;
-- @param_branch
SELECT CASE WHEN id = 1 THEN status ELSE @param END AS s FROM accounts;
-- @condition_is_not_a_branch
SELECT CASE WHEN status = @cond THEN plain ELSE '' END AS s FROM accounts;

-- @fetch_merged
SELECT l.id AS left_id, NULL AS right_id, 'published' AS row_status FROM left_rows l
UNION ALL
SELECT NULL AS left_id, r.id AS right_id, r.status AS row_status FROM right_rows r;

-- @scalar_subquery
SELECT (SELECT cid FROM accounts LIMIT 1) AS c FROM accounts;
-- @scalar_subquery_with_cte
SELECT (WITH c AS (SELECT cid FROM accounts) SELECT cid FROM c LIMIT 1) AS c FROM accounts;

-- @one_typed_one_not
SELECT plain FROM accounts WHERE cid = @cid AND id = @id;
-- @param_list
SELECT plain FROM accounts WHERE cid IN @cids;
-- @assignment
UPDATE accounts SET cid = @cid WHERE id = @id;
-- @insert_from_select
INSERT INTO accounts (id, cid, status, plain) SELECT id, @cid, status, plain FROM accounts;

-- @get_status
SELECT status FROM codec_rows WHERE id = @id;
-- @set_status
UPDATE codec_rows SET status = @status WHERE id = @id;

-- @spelled_with_on
SELECT named_owned.id FROM named_owned JOIN named_owners ON named_owned.id = named_owners.id;
-- @spelled_with_using
SELECT named_owned.id FROM named_owned JOIN named_owners USING (id);
-- @spelled_naturally
SELECT named_owned.id FROM named_owned NATURAL JOIN named_owners;
-- @outer_join_by_name_stays_silent
SELECT named_owned.id FROM named_owned LEFT JOIN named_owners USING (id);

-- @param_meets_two_domains
SELECT named_owners.title FROM named_owners, other_domain
WHERE named_owners.id = @needle AND other_domain.id = @needle;
