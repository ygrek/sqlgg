`-diff` replays both schemas against a reset state. Functions declared with
CREATE FUNCTION are part of that state: the same declaration in `-base` and
`-target` must not clash ("already registered").

  $ cat > base.sql <<'EOF'
  > CREATE FUNCTION f(x INT) RETURNS INT AS 'select 1';
  > CREATE TABLE t (id INT NOT NULL);
  > EOF

  $ cat > target.sql <<'EOF'
  > CREATE FUNCTION f(x INT) RETURNS INT AS 'select 1';
  > CREATE TABLE t (id INT NOT NULL, name TEXT);
  > EOF

  $ sqlgg -no-header -dialect mysql -diff -now 20260101000000 -gen sql -name mig -base base.sql -target target.sql
  -- [sqlgg] generated
  -- [sqlgg] id=20260101000000_alter_t_add_col_name
  ALTER TABLE `t` ADD COLUMN `name` TEXT;
  ALTER TABLE `t` DROP COLUMN `name`;
