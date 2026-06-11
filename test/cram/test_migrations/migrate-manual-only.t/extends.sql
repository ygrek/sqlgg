-- [sqlgg] manual
-- [sqlgg] id=20260609000000
ALTER TABLE users RENAME COLUMN email TO email_address;
ALTER TABLE users RENAME COLUMN email_address TO email;
