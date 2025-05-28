-- -- https://dev.mysql.com/doc/refman/5.7/en/json-function-reference.html

create table test ( foo JSON );

SELECT JSON_REMOVE('{"a":1,"b":2,"c":3}','$.b');
--> '{"a":1,"c":3}'

SELECT JSON_REMOVE('{"a":1,"b":2,"c":3}','$.b','$.c');
--> '{"a":1}'

SELECT JSON_ARRAY('a','b','c');
--> '["a","b","c"]'

SELECT JSON_SET('{"a":1,"b":2,"c":3}','$.b','foo');
--> '{"a":1,"b":"foo","c":3}'
