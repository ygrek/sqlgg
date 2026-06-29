ALTER TABLE users ADD COLUMN status INT NOT NULL DEFAULT 1;
ALTER TABLE users ADD UNIQUE INDEX uniq_email (email);
ALTER TABLE post_tags ADD INDEX idx_tag (tag_id);
