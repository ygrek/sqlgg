CREATE TABLE a (aid INT NOT NULL PRIMARY KEY, bk INT NULL);
CREATE TABLE b (bid INT NOT NULL PRIMARY KEY, ck INT NULL, bname TEXT NOT NULL);
CREATE TABLE c (cid INT NOT NULL PRIMARY KEY, cname TEXT NOT NULL);

-- [sqlgg] dynamic_select=true
-- @chain
SELECT a.aid, b.bid, b.bname, c.cid, c.cname
FROM a LEFT JOIN b ON b.bid = a.bk LEFT JOIN c ON c.cid = b.ck
WHERE a.aid > @min_id;

-- [sqlgg] dynamic_select=true
-- @left_then_inner
SELECT a.aid, b.bid, b.bname, c.cid, c.cname
FROM a LEFT JOIN b ON b.bid = a.bk JOIN c ON c.cid = b.ck
WHERE a.aid > @min_id;
