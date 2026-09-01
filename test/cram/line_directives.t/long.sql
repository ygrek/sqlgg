CREATE TABLE wide (
  c1 INT NOT NULL, c2 INT NOT NULL, c3 INT NOT NULL, c4 INT NOT NULL,
  c5 INT NOT NULL, c6 INT NOT NULL, c7 INT NOT NULL, c8 INT NOT NULL,
  -- [sqlgg] module=No_such_codec
  bad INT NOT NULL
);

-- @big
UPDATE wide
   SET c1 = @p1, c2 = @p2, c3 = @p3, c4 = @p4,
       c5 = @p5, c6 = @p6, c7 = @p7, c8 = @p8
 WHERE bad = @bad;

-- @after
DELETE FROM wide;
