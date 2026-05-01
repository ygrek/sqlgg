CREATE TABLE `foo` (
  `col1` INTEGER NOT NULL,
  `col2` INTEGER NOT NULL,
  `col3` INTEGER NOT NULL
);
CREATE INDEX `foo_unique` ON `foo` (`col1`, `col2`, `col3`);

ALTER TABLE `foo`
DROP INDEX `foo_unique`,
ADD UNIQUE `foo_unique` (`col1`, `col3`);

CREATE TABLE "bar" (
  "id" INTEGER NOT NULL,
  "role" SMALLINT NOT NULL,
  "role_new" INTEGER NOT NULL,
  "omniscience" INTEGER NOT NULL
);

ALTER TABLE "bar"
  DROP COLUMN "role",
  ALTER COLUMN "role_new" TYPE SMALLINT,
  ALTER COLUMN "role_new" SET NOT NULL,
  ALTER COLUMN "omniscience" TYPE BOOLEAN,
  ALTER COLUMN "omniscience" SET NOT NULL;

ALTER TABLE "bar" RENAME COLUMN "role_new" TO "role";
