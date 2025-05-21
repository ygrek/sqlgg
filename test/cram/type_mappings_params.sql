CREATE TABLE example (
-- [sqlgg] module=ExampleId  
    id INT AUTO_INCREMENT PRIMARY KEY,
-- [sqlgg] module=Name
    name VARCHAR(255) NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
SELECT id FROM example WHERE (name, id) IN @x;

CREATE TABLE example2 (
    id INT AUTO_INCREMENT PRIMARY KEY,
-- [sqlgg] module=Name2
    name_2 VARCHAR(255) NOT NULL
);

SELECT example2.id, name_2 
FROM example
JOIN example2 ON example.id = example2.id
WHERE name IN @name AND example2.name_2 IN @name_2 AND example.id = @id;
