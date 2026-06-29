-- [sqlgg] manual
-- [sqlgg] id=20260101120001
ALTER TABLE users ADD COLUMN status INT NOT NULL DEFAULT 1;
ALTER TABLE users DROP COLUMN status;
