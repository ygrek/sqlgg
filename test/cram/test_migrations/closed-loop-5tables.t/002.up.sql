ALTER TABLE posts ADD COLUMN created_at INT;
ALTER TABLE comments ADD COLUMN created_at INT;
ALTER TABLE post_tags ADD KEY idx_tag (tag_id);
