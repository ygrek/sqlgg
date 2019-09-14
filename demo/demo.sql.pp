-- @create_person
CREATE TABLE IF NOT EXISTS person (id INTEGER PRIMARY KEY %AUTO_INCREMENT%AUTOINCREMENT%,name TEXT,surname TEXT NULL);
-- @add_person
INSERT INTO person (name,surname) VALUES;

-- @create_money
CREATE TABLE IF NOT EXISTS money (src INTEGER, dst INTEGER, amount INTEGER);
-- @add_money
INSERT INTO money VALUES;

-- @calc_total
SELECT %CONCAT(name,' ',surname)%name || ' ' || surname% AS fullname, SUM(amount) as total FROM person JOIN money ON src = id GROUP BY id;
-- @list_donors
SELECT DISTINCT surname FROM person JOIN money ON src = id AND dst = (SELECT id FROM person WHERE surname LIKE ?) LIMIT ?;

DROP TABLE IF EXISTS person;
DROP TABLE IF EXISTS money;
