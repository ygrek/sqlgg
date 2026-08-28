CREATE TABLE files (id INT, data BLOB, name TEXT);

-- @by_data
SELECT id FROM files WHERE data IN @datas;

-- @by_name
SELECT id FROM files WHERE name IN @names;
