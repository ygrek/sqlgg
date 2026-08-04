CREATE TABLE item (iid INT NOT NULL PRIMARY KEY, tag TEXT NULL, sk INT NULL);
CREATE TABLE stock (sid INT NOT NULL PRIMARY KEY, place TEXT NOT NULL, hits INT NULL);

-- [sqlgg] dynamic_select=true
-- @wide
SELECT i.iid, i.tag, s.sid, s.place, s.hits
FROM item i LEFT JOIN stock s ON s.sid = i.sk
WHERE i.iid > @min_id;
