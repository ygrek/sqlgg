Basic migrations - ADD COLUMN inverse is DROP:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE users (id INT NOT NULL, name TEXT);
  > ALTER TABLE users ADD COLUMN age INT NOT NULL;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_users_1 db  =
      T.execute db ("ALTER TABLE users ADD COLUMN age INT NOT NULL") T.no_params
  
    let revert_alter_users_1 db  =
      T.execute db ("ALTER TABLE `users` DROP COLUMN `age`") T.no_params
  
    let migrations = [
      ("alter_users_1", [(apply_alter_users_1, revert_alter_users_1)]);
    ]
  
  end (* module Mig *)

DROP COLUMN inverse is ADD with reconstructed column definition:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE users (id INT NOT NULL, name TEXT, age INT NOT NULL);
  > ALTER TABLE users DROP COLUMN age;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_users_1 db  =
      T.execute db ("ALTER TABLE users DROP COLUMN age") T.no_params
  
    let revert_alter_users_1 db  =
      T.execute db ("ALTER TABLE `users` ADD COLUMN `age` INT NOT NULL") T.no_params
  
    let migrations = [
      ("alter_users_1", [(apply_alter_users_1, revert_alter_users_1)]);
    ]
  
  end (* module Mig *)

CHANGE COLUMN inverse restores old column definition:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE users (id INT NOT NULL, name TEXT);
  > ALTER TABLE users CHANGE COLUMN name full_name TEXT NOT NULL;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_users_1 db  =
      T.execute db ("ALTER TABLE users CHANGE COLUMN name full_name TEXT NOT NULL") T.no_params
  
    let revert_alter_users_1 db  =
      T.execute db ("ALTER TABLE `users` CHANGE COLUMN `full_name` `name` TEXT") T.no_params
  
    let migrations = [
      ("alter_users_1", [(apply_alter_users_1, revert_alter_users_1)]);
    ]
  
  end (* module Mig *)

RENAME COLUMN inverse swaps old and new names:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE users (id INT NOT NULL, email TEXT);
  > ALTER TABLE users RENAME COLUMN email TO mail;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_users_1 db  =
      T.execute db ("ALTER TABLE users RENAME COLUMN email TO mail") T.no_params
  
    let revert_alter_users_1 db  =
      T.execute db ("ALTER TABLE `users` RENAME COLUMN `mail` TO `email`") T.no_params
  
    let migrations = [
      ("alter_users_1", [(apply_alter_users_1, revert_alter_users_1)]);
    ]
  
  end (* module Mig *)

RENAME TABLE inverse uses new name and renames back to old:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE users (id INT NOT NULL);
  > ALTER TABLE users RENAME TO accounts;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_users_1 db  =
      T.execute db ("ALTER TABLE users RENAME TO accounts") T.no_params
  
    let revert_alter_users_1 db  =
      T.execute db ("ALTER TABLE `accounts` RENAME TO `users`") T.no_params
  
    let migrations = [
      ("alter_users_1", [(apply_alter_users_1, revert_alter_users_1)]);
    ]
  
  end (* module Mig *)

Multi-action ALTER inverts all actions in reverse order:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (a INT NOT NULL, b TEXT, c INT NOT NULL);
  > ALTER TABLE t DROP COLUMN b, ADD COLUMN d TEXT NOT NULL, RENAME COLUMN a TO aa;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN b, ADD COLUMN d TEXT NOT NULL, RENAME COLUMN a TO aa") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` RENAME COLUMN `aa` TO `a`, DROP COLUMN `d`, ADD COLUMN `b` TEXT") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

Multiple ALTER statements produce a list of migrations:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE items (id INT NOT NULL, name TEXT, price INT NOT NULL);
  > ALTER TABLE items ADD COLUMN stock INT NOT NULL;
  > ALTER TABLE items DROP COLUMN price;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_items_1 db  =
      T.execute db ("ALTER TABLE items ADD COLUMN stock INT NOT NULL") T.no_params
  
    let revert_alter_items_1 db  =
      T.execute db ("ALTER TABLE `items` DROP COLUMN `stock`") T.no_params
  
    let apply_alter_items_2 db  =
      T.execute db ("ALTER TABLE items DROP COLUMN price") T.no_params
  
    let revert_alter_items_2 db  =
      T.execute db ("ALTER TABLE `items` ADD COLUMN `price` INT NOT NULL") T.no_params
  
    let migrations = [
      ("alter_items_1", [(apply_alter_items_1, revert_alter_items_1)]);
      ("alter_items_2", [(apply_alter_items_2, revert_alter_items_2)]);
    ]
  
  end (* module Mig *)

DDL and migrations in separate files (like real usage):
  $ cat > ddl.sql <<'EOF'
  > CREATE TABLE products (id INT NOT NULL, name TEXT, weight FLOAT, active BOOLEAN NOT NULL);
  > EOF
  $ cat > migrations.sql <<'EOF'
  > ALTER TABLE products ADD COLUMN color TEXT;
  > ALTER TABLE products DROP COLUMN weight;
  > ALTER TABLE products RENAME COLUMN active TO enabled;
  > EOF
  $ sqlgg -no-header -gen none ddl.sql -gen caml -migrations -name mig -dialect mysql migrations.sql
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_products_0 db  =
      T.execute db ("ALTER TABLE products ADD COLUMN color TEXT") T.no_params
  
    let revert_alter_products_0 db  =
      T.execute db ("ALTER TABLE `products` DROP COLUMN `color`") T.no_params
  
    let apply_alter_products_1 db  =
      T.execute db ("ALTER TABLE products DROP COLUMN weight") T.no_params
  
    let revert_alter_products_1 db  =
      T.execute db ("ALTER TABLE `products` ADD COLUMN `weight` FLOAT") T.no_params
  
    let apply_alter_products_2 db  =
      T.execute db ("ALTER TABLE products RENAME COLUMN active TO enabled") T.no_params
  
    let revert_alter_products_2 db  =
      T.execute db ("ALTER TABLE `products` RENAME COLUMN `enabled` TO `active`") T.no_params
  
    let migrations = [
      ("alter_products_0", [(apply_alter_products_0, revert_alter_products_0)]);
      ("alter_products_1", [(apply_alter_products_1, revert_alter_products_1)]);
      ("alter_products_2", [(apply_alter_products_2, revert_alter_products_2)]);
    ]
  
  end (* module Mig *)

ENUM column - DROP inverse reconstructs ENUM type:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE orders (id INT NOT NULL, status ENUM('pending','shipped','delivered') NOT NULL);
  > ALTER TABLE orders DROP COLUMN status;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_orders_1 db  =
      T.execute db ("ALTER TABLE orders DROP COLUMN status") T.no_params
  
    let revert_alter_orders_1 db  =
      T.execute db ("ALTER TABLE `orders` ADD COLUMN `status` ENUM('delivered', 'pending', 'shipped') NOT NULL") T.no_params
  
    let migrations = [
      ("alter_orders_1", [(apply_alter_orders_1, revert_alter_orders_1)]);
    ]
  
  end (* module Mig *)

ENUM column - ADD inverse is DROP (trivial):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE orders (id INT NOT NULL);
  > ALTER TABLE orders ADD COLUMN priority ENUM('low','medium','high') NOT NULL;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_orders_1 db  =
      T.execute db ("ALTER TABLE orders ADD COLUMN priority ENUM('low','medium','high') NOT NULL") T.no_params
  
    let revert_alter_orders_1 db  =
      T.execute db ("ALTER TABLE `orders` DROP COLUMN `priority`") T.no_params
  
    let migrations = [
      ("alter_orders_1", [(apply_alter_orders_1, revert_alter_orders_1)]);
    ]
  
  end (* module Mig *)

ENUM column - CHANGE preserves old ENUM type in inverse:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE orders (id INT NOT NULL, status ENUM('pending','shipped') NOT NULL);
  > ALTER TABLE orders CHANGE COLUMN status order_status ENUM('pending','shipped','delivered','returned') NOT NULL;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_orders_1 db  =
      T.execute db ("ALTER TABLE orders CHANGE COLUMN status order_status ENUM('pending','shipped','delivered','returned') NOT NULL") T.no_params
  
    let revert_alter_orders_1 db  =
      T.execute db ("ALTER TABLE `orders` CHANGE COLUMN `order_status` `status` ENUM('pending', 'shipped') NOT NULL") T.no_params
  
    let migrations = [
      ("alter_orders_1", [(apply_alter_orders_1, revert_alter_orders_1)]);
    ]
  
  end (* module Mig *)

ALTER TABLE ADD INDEX (named) generates DROP INDEX as inverse:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, name TEXT);
  > ALTER TABLE t ADD INDEX idx_name (name);
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD INDEX idx_name (name)") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` DROP INDEX `idx_name`") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

ALTER TABLE DROP INDEX is non-invertible (columns unknown), requires down=explicit:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql - 2>&1
  > CREATE TABLE t (id INT NOT NULL, name TEXT);
  > ALTER TABLE t DROP INDEX idx_name;
  > EOF
  migrations mode: alter_t_1 contains non-invertible actions (index/constraint ops), use -- [sqlgg] down=explicit
  Errors encountered, no code generated
  [1]

ALTER TABLE DROP INDEX with down=explicit works:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, name TEXT);
  > -- [sqlgg] down=explicit
  > ALTER TABLE t DROP INDEX idx_name;
  > ALTER TABLE t ADD INDEX idx_name (name);
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP INDEX idx_name") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD INDEX idx_name (name)") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

Compound ALTER with ADD INDEX and column ops - all invertible:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, name TEXT);
  > ALTER TABLE t ADD COLUMN age INT, ADD INDEX idx_name (name);
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN age INT, ADD INDEX idx_name (name)") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` DROP INDEX `idx_name`, DROP COLUMN `age`") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

ALTER TABLE ADD CONSTRAINT (named) generates DROP CONSTRAINT as inverse:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, email TEXT);
  > ALTER TABLE t ADD CONSTRAINT chk_email CHECK (email IS NOT NULL);
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD CONSTRAINT chk_email CHECK (email IS NOT NULL)") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` DROP CONSTRAINT `chk_email`") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

ALTER TABLE DROP FOREIGN KEY is non-invertible, requires down=explicit:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql - 2>&1
  > CREATE TABLE t (id INT NOT NULL);
  > ALTER TABLE t DROP FOREIGN KEY fk_foo;
  > EOF
  migrations mode: alter_t_1 contains non-invertible actions (index/constraint ops), use -- [sqlgg] down=explicit
  Errors encountered, no code generated
  [1]

ALTER TABLE DROP CHECK is non-invertible, requires down=explicit:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql - 2>&1
  > CREATE TABLE t (id INT NOT NULL);
  > ALTER TABLE t DROP CHECK chk_foo;
  > EOF
  migrations mode: alter_t_1 contains non-invertible actions (index/constraint ops), use -- [sqlgg] down=explicit
  Errors encountered, no code generated
  [1]

ALTER TABLE DROP FOREIGN KEY with down=explicit works:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL);
  > -- [sqlgg] down=explicit
  > ALTER TABLE t DROP FOREIGN KEY fk_foo;
  > ALTER TABLE t ADD CONSTRAINT fk_foo FOREIGN KEY (id) REFERENCES other(id);
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP FOREIGN KEY fk_foo") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD CONSTRAINT fk_foo FOREIGN KEY (id) REFERENCES other(id)") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

Compound ALTER with DROP INDEX (non-invertible) errors without down=explicit:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql - 2>&1
  > CREATE TABLE t (id INT NOT NULL, data TEXT);
  > ALTER TABLE t DROP COLUMN data, DROP INDEX idx_id;
  > EOF
  migrations mode: alter_t_1 contains non-invertible actions (index/constraint ops), use -- [sqlgg] down=explicit
  Errors encountered, no code generated
  [1]

Compound ALTER with DROP INDEX (non-invertible) works with down=explicit:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, data TEXT);
  > -- [sqlgg] down=explicit
  > ALTER TABLE t DROP COLUMN data, DROP INDEX idx_id;
  > ALTER TABLE t ADD INDEX idx_id (data), ADD COLUMN data TEXT;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN data, DROP INDEX idx_id") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD INDEX idx_id (data), ADD COLUMN data TEXT") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

Manual down via down=explicit, next statement is the down migration:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, data TEXT);
  > -- [sqlgg] down=explicit
  > -- @add_idx
  > ALTER TABLE t ADD INDEX idx_data (data);
  > ALTER TABLE t DROP INDEX idx_data;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_add_idx db  =
      T.execute db ("ALTER TABLE t ADD INDEX idx_data (data)") T.no_params
  
    let revert_add_idx db  =
      T.execute db ("ALTER TABLE t DROP INDEX idx_data") T.no_params
  
    let migrations = [
      ("add_idx", [(apply_add_idx, revert_add_idx)]);
    ]
  
  end (* module Mig *)

MODIFY COLUMN (parsed as CHANGE) inverse restores old definition:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, val TEXT);
  > ALTER TABLE t MODIFY COLUMN val INT NOT NULL;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t MODIFY COLUMN val INT NOT NULL") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` CHANGE COLUMN `val` `val` TEXT") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

RENAME INDEX inverse swaps old and new names:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL);
  > ALTER TABLE t RENAME INDEX old_idx TO new_idx;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t RENAME INDEX old_idx TO new_idx") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` RENAME INDEX `new_idx` TO `old_idx`") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

ADD COLUMN with position (AFTER/FIRST):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, name TEXT);
  > ALTER TABLE t ADD COLUMN age INT AFTER id;
  > ALTER TABLE t ADD COLUMN flag INT FIRST;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN age INT AFTER id") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` DROP COLUMN `age`") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN flag INT FIRST") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` DROP COLUMN `flag`") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
    ]
  
  end (* module Mig *)

Sequential ALTERs - schema evolves, second DROP sees updated schema:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL);
  > ALTER TABLE t ADD COLUMN x TEXT NOT NULL;
  > ALTER TABLE t DROP COLUMN x;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN x TEXT NOT NULL") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` DROP COLUMN `x`") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN x") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `x` TEXT NOT NULL") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
    ]
  
  end (* module Mig *)

Mixed down=explicit and auto-computed in same file:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, a TEXT);
  > ALTER TABLE t ADD COLUMN b INT NOT NULL;
  > -- [sqlgg] down=explicit
  > ALTER TABLE t ADD INDEX idx_a (a);
  > ALTER TABLE t DROP INDEX idx_a;
  > ALTER TABLE t DROP COLUMN a;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t ADD COLUMN b INT NOT NULL") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` DROP COLUMN `b`") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t ADD INDEX idx_a (a)") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE t DROP INDEX idx_a") T.no_params
  
    let apply_alter_t_3 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN a") T.no_params
  
    let revert_alter_t_3 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `a` TEXT") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
      ("alter_t_3", [(apply_alter_t_3, revert_alter_t_3)]);
    ]
  
  end (* module Mig *)

CREATE INDEX generates DROP INDEX as down:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, name TEXT);
  > CREATE INDEX idx_name ON t (name);
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_create_index_idx_name db  =
      T.execute db ("CREATE INDEX idx_name ON t (name)") T.no_params
  
    let revert_create_index_idx_name db  =
      T.execute db ("DROP INDEX `idx_name` ON `t`") T.no_params
  
    let migrations = [
      ("create_index_idx_name", [(apply_create_index_idx_name, revert_create_index_idx_name)]);
    ]
  
  end (* module Mig *)

Standalone RENAME TABLE generates inverse rename:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE old_t (id INT NOT NULL);
  > RENAME TABLE old_t TO new_t;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_old_t_1 db  =
      T.execute db ("RENAME TABLE old_t TO new_t") T.no_params
  
    let revert_alter_old_t_1 db  =
      T.execute db ("RENAME TABLE `new_t` TO `old_t`") T.no_params
  
    let migrations = [
      ("alter_old_t_1", [(apply_alter_old_t_1, revert_alter_old_t_1)]);
    ]
  
  end (* module Mig *)

DROP TABLE requires down=explicit:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql - 2>&1
  > CREATE TABLE t (id INT NOT NULL);
  > DROP TABLE t;
  > EOF
  migrations mode: drop_t contains non-invertible actions (index/constraint ops), use -- [sqlgg] down=explicit
  Errors encountered, no code generated
  [1]

Full migration scenario - DDL file then migrations file:
  $ cat > schema.sql <<'EOF'
  > CREATE TABLE accounts (id INT NOT NULL, email TEXT NOT NULL, status ENUM('active','inactive') NOT NULL);
  > CREATE INDEX idx_email ON accounts (email);
  > EOF
  $ cat > mig.sql <<'EOF'
  > ALTER TABLE accounts ADD COLUMN name TEXT;
  > ALTER TABLE accounts DROP COLUMN status;
  > ALTER TABLE accounts RENAME COLUMN email TO login;
  > CREATE INDEX idx_login ON accounts (login);
  > EOF
  $ sqlgg -no-header -gen none schema.sql -gen caml -migrations -name mig -dialect mysql mig.sql
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_accounts_0 db  =
      T.execute db ("ALTER TABLE accounts ADD COLUMN name TEXT") T.no_params
  
    let revert_alter_accounts_0 db  =
      T.execute db ("ALTER TABLE `accounts` DROP COLUMN `name`") T.no_params
  
    let apply_alter_accounts_1 db  =
      T.execute db ("ALTER TABLE accounts DROP COLUMN status") T.no_params
  
    let revert_alter_accounts_1 db  =
      T.execute db ("ALTER TABLE `accounts` ADD COLUMN `status` ENUM('active', 'inactive') NOT NULL") T.no_params
  
    let apply_alter_accounts_2 db  =
      T.execute db ("ALTER TABLE accounts RENAME COLUMN email TO login") T.no_params
  
    let revert_alter_accounts_2 db  =
      T.execute db ("ALTER TABLE `accounts` RENAME COLUMN `login` TO `email`") T.no_params
  
    let apply_create_index_idx_login db  =
      T.execute db ("CREATE INDEX idx_login ON accounts (login)") T.no_params
  
    let revert_create_index_idx_login db  =
      T.execute db ("DROP INDEX `idx_login` ON `accounts`") T.no_params
  
    let migrations = [
      ("alter_accounts_0", [(apply_alter_accounts_0, revert_alter_accounts_0)]);
      ("alter_accounts_1", [(apply_alter_accounts_1, revert_alter_accounts_1)]);
      ("alter_accounts_2", [(apply_alter_accounts_2, revert_alter_accounts_2)]);
      ("create_index_idx_login", [(apply_create_index_idx_login, revert_create_index_idx_login)]);
    ]
  
  end (* module Mig *)

Numeric types roundtrip - DROP inverse reconstructs SQL types from schema:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (
  >   a INT NOT NULL,
  >   b INT UNSIGNED NOT NULL,
  >   c BIGINT NOT NULL,
  >   d BIGINT UNSIGNED NOT NULL,
  >   e TINYINT NOT NULL,
  >   f SMALLINT NOT NULL,
  >   g MEDIUMINT NOT NULL,
  >   h FLOAT NOT NULL,
  >   i DOUBLE NOT NULL,
  >   j DECIMAL(10,2) NOT NULL,
  >   k DECIMAL(5) NOT NULL,
  >   l DECIMAL NOT NULL,
  >   m BOOLEAN NOT NULL,
  >   n TEXT,
  >   o BLOB,
  >   p DATETIME NOT NULL,
  >   q JSON NOT NULL,
  >   r TINYBLOB,
  >   s MEDIUMBLOB,
  >   t2 LONGBLOB,
  >   u TINYTEXT,
  >   v MEDIUMTEXT,
  >   w LONGTEXT
  > );
  > ALTER TABLE t DROP COLUMN a;
  > ALTER TABLE t DROP COLUMN b;
  > ALTER TABLE t DROP COLUMN c;
  > ALTER TABLE t DROP COLUMN d;
  > ALTER TABLE t DROP COLUMN e;
  > ALTER TABLE t DROP COLUMN f;
  > ALTER TABLE t DROP COLUMN g;
  > ALTER TABLE t DROP COLUMN h;
  > ALTER TABLE t DROP COLUMN i;
  > ALTER TABLE t DROP COLUMN j;
  > ALTER TABLE t DROP COLUMN k;
  > ALTER TABLE t DROP COLUMN l;
  > ALTER TABLE t DROP COLUMN m;
  > ALTER TABLE t DROP COLUMN n;
  > ALTER TABLE t DROP COLUMN o;
  > ALTER TABLE t DROP COLUMN p;
  > ALTER TABLE t DROP COLUMN q;
  > ALTER TABLE t DROP COLUMN r;
  > ALTER TABLE t DROP COLUMN s;
  > ALTER TABLE t DROP COLUMN t2;
  > ALTER TABLE t DROP COLUMN u;
  > ALTER TABLE t DROP COLUMN v;
  > ALTER TABLE t DROP COLUMN w;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN a") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `a` INT NOT NULL") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN b") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `b` INT UNSIGNED NOT NULL") T.no_params
  
    let apply_alter_t_3 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN c") T.no_params
  
    let revert_alter_t_3 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `c` BIGINT NOT NULL") T.no_params
  
    let apply_alter_t_4 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN d") T.no_params
  
    let revert_alter_t_4 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `d` BIGINT UNSIGNED NOT NULL") T.no_params
  
    let apply_alter_t_5 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN e") T.no_params
  
    let revert_alter_t_5 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `e` TINYINT NOT NULL") T.no_params
  
    let apply_alter_t_6 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN f") T.no_params
  
    let revert_alter_t_6 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `f` SMALLINT NOT NULL") T.no_params
  
    let apply_alter_t_7 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN g") T.no_params
  
    let revert_alter_t_7 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `g` MEDIUMINT NOT NULL") T.no_params
  
    let apply_alter_t_8 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN h") T.no_params
  
    let revert_alter_t_8 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `h` FLOAT NOT NULL") T.no_params
  
    let apply_alter_t_9 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN i") T.no_params
  
    let revert_alter_t_9 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `i` DOUBLE NOT NULL") T.no_params
  
    let apply_alter_t_10 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN j") T.no_params
  
    let revert_alter_t_10 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `j` DECIMAL(10,2) NOT NULL") T.no_params
  
    let apply_alter_t_11 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN k") T.no_params
  
    let revert_alter_t_11 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `k` DECIMAL(5) NOT NULL") T.no_params
  
    let apply_alter_t_12 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN l") T.no_params
  
    let revert_alter_t_12 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `l` DECIMAL NOT NULL") T.no_params
  
    let apply_alter_t_13 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN m") T.no_params
  
    let revert_alter_t_13 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `m` BOOLEAN NOT NULL") T.no_params
  
    let apply_alter_t_14 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN n") T.no_params
  
    let revert_alter_t_14 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `n` TEXT") T.no_params
  
    let apply_alter_t_15 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN o") T.no_params
  
    let revert_alter_t_15 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `o` BLOB") T.no_params
  
    let apply_alter_t_16 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN p") T.no_params
  
    let revert_alter_t_16 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `p` DATETIME NOT NULL") T.no_params
  
    let apply_alter_t_17 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN q") T.no_params
  
    let revert_alter_t_17 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `q` JSON NOT NULL") T.no_params
  
    let apply_alter_t_18 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN r") T.no_params
  
    let revert_alter_t_18 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `r` TINYBLOB") T.no_params
  
    let apply_alter_t_19 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN s") T.no_params
  
    let revert_alter_t_19 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `s` MEDIUMBLOB") T.no_params
  
    let apply_alter_t_20 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN t2") T.no_params
  
    let revert_alter_t_20 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `t2` LONGBLOB") T.no_params
  
    let apply_alter_t_21 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN u") T.no_params
  
    let revert_alter_t_21 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `u` TINYTEXT") T.no_params
  
    let apply_alter_t_22 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN v") T.no_params
  
    let revert_alter_t_22 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `v` MEDIUMTEXT") T.no_params
  
    let apply_alter_t_23 db  =
      T.execute db ("ALTER TABLE t DROP COLUMN w") T.no_params
  
    let revert_alter_t_23 db  =
      T.execute db ("ALTER TABLE `t` ADD COLUMN `w` LONGTEXT") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
      ("alter_t_3", [(apply_alter_t_3, revert_alter_t_3)]);
      ("alter_t_4", [(apply_alter_t_4, revert_alter_t_4)]);
      ("alter_t_5", [(apply_alter_t_5, revert_alter_t_5)]);
      ("alter_t_6", [(apply_alter_t_6, revert_alter_t_6)]);
      ("alter_t_7", [(apply_alter_t_7, revert_alter_t_7)]);
      ("alter_t_8", [(apply_alter_t_8, revert_alter_t_8)]);
      ("alter_t_9", [(apply_alter_t_9, revert_alter_t_9)]);
      ("alter_t_10", [(apply_alter_t_10, revert_alter_t_10)]);
      ("alter_t_11", [(apply_alter_t_11, revert_alter_t_11)]);
      ("alter_t_12", [(apply_alter_t_12, revert_alter_t_12)]);
      ("alter_t_13", [(apply_alter_t_13, revert_alter_t_13)]);
      ("alter_t_14", [(apply_alter_t_14, revert_alter_t_14)]);
      ("alter_t_15", [(apply_alter_t_15, revert_alter_t_15)]);
      ("alter_t_16", [(apply_alter_t_16, revert_alter_t_16)]);
      ("alter_t_17", [(apply_alter_t_17, revert_alter_t_17)]);
      ("alter_t_18", [(apply_alter_t_18, revert_alter_t_18)]);
      ("alter_t_19", [(apply_alter_t_19, revert_alter_t_19)]);
      ("alter_t_20", [(apply_alter_t_20, revert_alter_t_20)]);
      ("alter_t_21", [(apply_alter_t_21, revert_alter_t_21)]);
      ("alter_t_22", [(apply_alter_t_22, revert_alter_t_22)]);
      ("alter_t_23", [(apply_alter_t_23, revert_alter_t_23)]);
    ]
  
  end (* module Mig *)

XML output - basic ADD COLUMN:
  $ cat <<'EOF' | sqlgg -no-header -gen xml -migrations -name mig -dialect mysql -
  > CREATE TABLE users (id INT NOT NULL, name TEXT);
  > ALTER TABLE users ADD COLUMN age INT NOT NULL;
  > EOF
  <?xml version="1.0"?>
  
  <migrations>
   <migration name="alter_users_1" apply="ALTER TABLE users ADD COLUMN age INT NOT NULL" revert="ALTER TABLE `users` DROP COLUMN `age`"/>
  </migrations>

XML output - multiple migrations:
  $ cat <<'EOF' | sqlgg -no-header -gen xml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (a INT NOT NULL, b TEXT, c INT NOT NULL);
  > ALTER TABLE t DROP COLUMN b, ADD COLUMN d TEXT NOT NULL, RENAME COLUMN a TO aa;
  > ALTER TABLE t RENAME TO t2;
  > EOF
  <?xml version="1.0"?>
  
  <migrations>
   <migration name="alter_t_1" apply="ALTER TABLE t DROP COLUMN b, ADD COLUMN d TEXT NOT NULL, RENAME COLUMN a TO aa" revert="ALTER TABLE `t` RENAME COLUMN `aa` TO `a`, DROP COLUMN `d`, ADD COLUMN `b` TEXT"/>
   <migration name="alter_t_2" apply="ALTER TABLE t RENAME TO t2" revert="ALTER TABLE `t2` RENAME TO `t`"/>
  </migrations>

XML output - DDL and migrations in separate files:
  $ cat > ddl.sql <<'EOF'
  > CREATE TABLE products (id INT NOT NULL, name TEXT, weight FLOAT);
  > EOF
  $ cat > migrations.sql <<'EOF'
  > ALTER TABLE products ADD COLUMN color TEXT;
  > ALTER TABLE products DROP COLUMN weight;
  > EOF
  $ sqlgg -no-header -dialect mysql -gen none ddl.sql -gen xml -migrations migrations.sql
  <?xml version="1.0"?>
  
  <migrations>
   <migration name="alter_products_0" apply="ALTER TABLE products ADD COLUMN color TEXT" revert="ALTER TABLE `products` DROP COLUMN `color`"/>
   <migration name="alter_products_1" apply="ALTER TABLE products DROP COLUMN weight" revert="ALTER TABLE `products` ADD COLUMN `weight` FLOAT"/>
  </migrations>

DROP PRIMARY KEY inverse is ADD PRIMARY KEY (single column):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE users (id INT NOT NULL PRIMARY KEY, name TEXT);
  > ALTER TABLE users DROP PRIMARY KEY;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_users_1 db  =
      T.execute db ("ALTER TABLE users DROP PRIMARY KEY") T.no_params
  
    let revert_alter_users_1 db  =
      T.execute db ("ALTER TABLE `users` ADD PRIMARY KEY (`id`)") T.no_params
  
    let migrations = [
      ("alter_users_1", [(apply_alter_users_1, revert_alter_users_1)]);
    ]
  
  end (* module Mig *)

DROP PRIMARY KEY inverse is ADD PRIMARY KEY (composite):
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (a INT NOT NULL, b INT NOT NULL, c TEXT, PRIMARY KEY (a, b));
  > ALTER TABLE t DROP PRIMARY KEY;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t DROP PRIMARY KEY") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` ADD PRIMARY KEY (`a`, `b`)") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
    ]
  
  end (* module Mig *)

XML output - special characters in SQL are escaped:
  $ cat <<'EOF' | sqlgg -no-header -gen xml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL);
  > ALTER TABLE t ADD COLUMN label TEXT;
  > EOF
  <?xml version="1.0"?>
  
  <migrations>
   <migration name="alter_t_1" apply="ALTER TABLE t ADD COLUMN label TEXT" revert="ALTER TABLE `t` DROP COLUMN `label`"/>
  </migrations>

CONVERT TO CHARACTER SET - inverse restores previous charset:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, name TEXT);
  > ALTER TABLE t CONVERT TO CHARACTER SET utf8;
  > ALTER TABLE t CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_alter_t_1 db  =
      T.execute db ("ALTER TABLE t CONVERT TO CHARACTER SET utf8") T.no_params
  
    let revert_alter_t_1 db  =
      T.execute db ("ALTER TABLE `t` (* unsupported: unknown previous charset *)") T.no_params
  
    let apply_alter_t_2 db  =
      T.execute db ("ALTER TABLE t CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci") T.no_params
  
    let revert_alter_t_2 db  =
      T.execute db ("ALTER TABLE `t` CONVERT TO CHARACTER SET utf8") T.no_params
  
    let migrations = [
      ("alter_t_1", [(apply_alter_t_1, revert_alter_t_1)]);
      ("alter_t_2", [(apply_alter_t_2, revert_alter_t_2)]);
    ]
  
  end (* module Mig *)

Non-migration statements (SELECT) produce an error:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql - 2>&1
  > CREATE TABLE t (id INT NOT NULL, x TEXT);
  > ALTER TABLE t ADD COLUMN y INT;
  > SELECT * FROM t;
  > EOF
  migrations mode: unsupported statement type: SELECT * FROM t
  Errors encountered, no code generated
  [1]

DML statements (INSERT/UPDATE/DELETE) require down=explicit:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql - 2>&1
  > CREATE TABLE t (id INT NOT NULL, x TEXT);
  > INSERT INTO t (id, x) VALUES (1, 'hello');
  > EOF
  migrations mode: insert_t_1 contains non-invertible actions (index/constraint ops), use -- [sqlgg] down=explicit
  Errors encountered, no code generated
  [1]

DML statements work with down=explicit:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, x TEXT);
  > -- [sqlgg] down=explicit
  > INSERT INTO t (id, x) VALUES (1, 'hello');
  > DELETE FROM t WHERE id = 1;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_insert_t_1 db  =
      T.execute db ("INSERT INTO t (id, x) VALUES (1, 'hello')") T.no_params
  
    let revert_insert_t_1 db  =
      T.execute db ("DELETE FROM t WHERE id = 1") T.no_params
  
    let migrations = [
      ("insert_t_1", [(apply_insert_t_1, revert_insert_t_1)]);
    ]
  
  end (* module Mig *)

UPDATE with down=explicit:
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql -
  > CREATE TABLE t (id INT NOT NULL, x TEXT);
  > -- [sqlgg] down=explicit
  > UPDATE t SET x = 'new' WHERE id = 1;
  > UPDATE t SET x = 'old' WHERE id = 1;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_update_t_1 db  =
      T.execute db ("UPDATE t SET x = 'new' WHERE id = 1") T.no_params
  
    let revert_update_t_1 db  =
      T.execute db ("UPDATE t SET x = 'old' WHERE id = 1") T.no_params
  
    let migrations = [
      ("update_t_1", [(apply_update_t_1, revert_update_t_1)]);
    ]
  
  end (* module Mig *)

Realistic migration scenario - DDL schema, then mixed ALTER/DML/index migrations:
  $ cat > schema.sql <<'EOF'
  > CREATE TABLE reports (
  >   id INT NOT NULL PRIMARY KEY,
  >   title TEXT,
  >   status TINYINT NOT NULL DEFAULT 0
  > );
  > EOF
  $ cat <<'EOF' | sqlgg -no-header -gen caml -migrations -name mig -dialect mysql schema.sql -migrations -
  > -- @add_archived
  > ALTER TABLE `reports` ADD COLUMN `archived` TINYINT NOT NULL DEFAULT 0;
  > -- @add_status_index
  > -- [sqlgg] down=explicit
  > ALTER TABLE `reports` ADD INDEX `idx_status` (`status`);
  > ALTER TABLE `reports` DROP INDEX `idx_status`;
  > -- @seed_defaults
  > -- [sqlgg] down=explicit
  > INSERT INTO `reports` (id, title, status) VALUES (1, 'default report', 1);
  > DELETE FROM `reports` WHERE id = 1;
  > -- @backfill_archived
  > -- [sqlgg] down=explicit
  > UPDATE `reports` SET `archived` = 1 WHERE `status` = 2;
  > UPDATE `reports` SET `archived` = 0 WHERE `archived` = 1;
  > EOF
  module Mig (T : Sqlgg_traits.M_io) = struct
  
    module IO = T.IO
  
    let apply_add_archived db  =
      T.execute db ("ALTER TABLE `reports` ADD COLUMN `archived` TINYINT NOT NULL DEFAULT 0") T.no_params
  
    let revert_add_archived db  =
      T.execute db ("ALTER TABLE `reports` DROP COLUMN `archived`") T.no_params
  
    let apply_add_status_index db  =
      T.execute db ("ALTER TABLE `reports` ADD INDEX `idx_status` (`status`)") T.no_params
  
    let revert_add_status_index db  =
      T.execute db ("ALTER TABLE `reports` DROP INDEX `idx_status`") T.no_params
  
    let apply_seed_defaults db  =
      T.execute db ("INSERT INTO `reports` (id, title, status) VALUES (1, 'default report', 1)") T.no_params
  
    let revert_seed_defaults db  =
      T.execute db ("DELETE FROM `reports` WHERE id = 1") T.no_params
  
    let apply_backfill_archived db  =
      T.execute db ("UPDATE `reports` SET `archived` = 1 WHERE `status` = 2") T.no_params
  
    let revert_backfill_archived db  =
      T.execute db ("UPDATE `reports` SET `archived` = 0 WHERE `archived` = 1") T.no_params
  
    let migrations = [
      ("add_archived", [(apply_add_archived, revert_add_archived)]);
      ("add_status_index", [(apply_add_status_index, revert_add_status_index)]);
      ("seed_defaults", [(apply_seed_defaults, revert_seed_defaults)]);
      ("backfill_archived", [(apply_backfill_archived, revert_backfill_archived)]);
    ]
  
  end (* module Mig *)
