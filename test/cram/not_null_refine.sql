CREATE TABLE items (
  id INT NOT NULL,
  name TEXT NULL,
  descr TEXT NULL
);

-- @static_not_null
SELECT name FROM items WHERE name IS NOT NULL;

-- [sqlgg] dynamic_select=true
-- @dynamic_not_null
SELECT name, descr FROM items WHERE name IS NOT NULL AND descr IS NOT NULL;

-- [sqlgg] dynamic_select=true
-- @dynamic_or_stays_nullable
SELECT name, descr FROM items WHERE name IS NOT NULL OR descr IS NOT NULL;
