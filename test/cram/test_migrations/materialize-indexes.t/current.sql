CREATE TABLE `t` (`a` INT NOT NULL, `b` INT NOT NULL, `body` TEXT NOT NULL, UNIQUE KEY `a` (`a`, `b`), KEY `b` (`b`));
ALTER TABLE `t` ADD FULLTEXT KEY `ft` (`body`);
