ALTER TABLE posts ADD COLUMN created_at INT;
ALTER TABLE users ADD UNIQUE KEY uniq_name (name);
