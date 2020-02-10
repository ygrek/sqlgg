CREATE TABLE database1.test (id INT PRIMARY KEY, txt TEXT);
CREATE TABLE database2.test (id INT PRIMARY KEY, amount INT);

SELECT COUNT(*)
FROM database1.test t1
FULL OUTER JOIN database2.test t2 ON t1.id = t2.id
WHERE t1.id IS NULL OR t2.id IS NULL;
