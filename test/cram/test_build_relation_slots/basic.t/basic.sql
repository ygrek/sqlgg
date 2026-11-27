CREATE TABLE users (id INT NOT NULL, name TEXT);
CREATE TABLE drafts (id INT, name TEXT NOT NULL);
CREATE TABLE external (id TEXT NOT NULL, name TEXT);
CREATE TABLE logs (msg TEXT NOT NULL);

-- @count_from
SELECT COUNT(1) AS n FROM @source AS source;

-- @select_id
SELECT source.id FROM @source AS source;

-- @count_by_id
SELECT COUNT(1) AS n FROM @source AS source WHERE source.id = @id;
