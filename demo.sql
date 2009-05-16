-- @create_person
CREATE TABLE person (id INTEGER PRIMARY KEY AUTOINCREMENT,name TEXT,surname TEXT);
-- @add_person
INSERT INTO person (name,surname) VALUES;

-- @create_money
CREATE TABLE money (src INTEGER, dst INTEGER, amount INTEGER);
-- @add_money
INSERT INTO money VALUES;

-- @calc_debit
SELECT name || ' ' || surname AS fullname, SUM(amount) as debit FROM person JOIN money ON src = id GROUP BY id;

