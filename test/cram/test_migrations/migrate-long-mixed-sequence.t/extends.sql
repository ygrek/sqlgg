-- [sqlgg] manual
-- [sqlgg] id=20251202
ALTER TABLE users ADD COLUMN bio TEXT;
ALTER TABLE users DROP COLUMN bio;

-- [sqlgg] manual
-- [sqlgg] id=20251204
ALTER TABLE posts ADD COLUMN body TEXT;
ALTER TABLE posts DROP COLUMN body;
