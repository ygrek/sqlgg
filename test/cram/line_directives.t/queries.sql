CREATE TABLE person (
  id INT NOT NULL,
  name TEXT NOT NULL
);

-- @count_persons
SELECT count(*) FROM person;

-- @rename
UPDATE person
   SET name = @name
 WHERE id = @id;

-- @erase
DELETE FROM person WHERE id = @id;
