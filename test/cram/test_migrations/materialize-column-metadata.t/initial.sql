CREATE TABLE plain (
  id INT NOT NULL
);

CREATE TABLE accounts (
  id INT NOT NULL,
  -- [sqlgg] module=Codecs.Cid
  cid BIGINT NOT NULL
);
