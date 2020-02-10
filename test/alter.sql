CREATE TABLE `foo` (
  `col1` INTEGER NOT NULL,
  `col2` INTEGER NOT NULL,
  `col3` INTEGER NOT NULL
);
CREATE INDEX `foo_unique` ON `foo` (`col1`, `col2`, `col3`);

ALTER TABLE `foo`
DROP INDEX `foo_unique`,
ADD UNIQUE `foo_unique` (`col1`, `col3`);
