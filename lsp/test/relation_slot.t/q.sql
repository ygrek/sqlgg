CREATE TABLE users (id INT NOT NULL, name TEXT, email TEXT);
CREATE TABLE drafts (id INT, name TEXT NOT NULL);
CREATE TABLE logs (msg TEXT NOT NULL);

-- @names
SELECT s.id, s.name FROM @source AS s WHERE s.id > @min;

-- @nothing_fits
SELECT s.missing FROM @source AS s;
