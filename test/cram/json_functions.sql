CREATE TABLE people (
  id INT AUTO_INCREMENT PRIMARY KEY,
  data JSON
);

CREATE TABLE people_data_json_kind_never_null_but_col_nullable (
  id INT AUTO_INCREMENT PRIMARY KEY,
-- [sqlgg] json_null_kind=false
  data JSON
);

CREATE TABLE people_data_json_kind_never_null_and_col_strict (
  id INT AUTO_INCREMENT PRIMARY KEY,
-- [sqlgg] json_null_kind=false
  data JSON NOT NULL
);

-- @one
INSERT INTO people (data) VALUES
  (JSON_OBJECT(
    'name', 'Alice',
    'age', 30,
    'skills', JSON_ARRAY('SQL', 'Python'),
    'address', JSON_OBJECT('city', 'Paris', 'zip', '75000')
  )),
  (JSON_OBJECT(
    'name', 'Bob',
    'age', 25,
    'skills', JSON_ARRAY('JavaScript'),
    'address', JSON_OBJECT('city', 'Berlin', 'zip', '10115')
  )),
  (JSON_OBJECT(
    'name', 'Charlie',
    'age', 35,
    'skills', JSON_ARRAY(),
    'address', JSON_OBJECT('city', 'New York', 'zip', '10001')
  )),
  (JSON_OBJECT(
    'name', NULL,
    'age', NULL
  ));

-- @two
SELECT id, JSON_EXTRACT(data, '$.name') AS name
FROM people
WHERE JSON_UNQUOTE(JSON_EXTRACT(data, '$.address.city')) = 'Paris';

-- @three
SELECT
  id,
  JSON_SET(data, '$.active', TRUE) AS data_with_active
FROM people;

-- @four
SELECT
  id,
  JSON_SET(data, '$.active', TRUE) AS data_with_active
FROM people_data_json_kind_never_null_but_col_nullable;

-- @five
SELECT
  id,
  JSON_SET(data, '$.active', TRUE) AS data_with_active
FROM people_data_json_kind_never_null_and_col_strict;
