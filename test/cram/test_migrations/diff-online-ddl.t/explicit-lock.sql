CREATE TABLE users (id INT NOT NULL, email VARCHAR(255));
ALTER TABLE users ADD INDEX email_idx (email), LOCK=NONE;
