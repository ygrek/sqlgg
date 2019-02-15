-- https://dev.mysql.com/doc/refman/8.0/en/string-functions.html#function_substring

SELECT SUBSTRING('Quadratically',5);
--> 'ratically'

SELECT SUBSTRING('foobarbar' FROM 4);
--> 'barbar'

SELECT SUBSTRING('Quadratically',5,6);
--> 'ratica'

SELECT SUBSTRING('Sakila', -3);
--> 'ila'

SELECT SUBSTRING('Sakila', -5, 3);
--> 'aki'

SELECT SUBSTRING('Sakila' FROM -4 FOR 2);
--> 'ki'
