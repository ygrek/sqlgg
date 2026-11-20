TiDB DDL that used to need a text fallback parses into IR

  $ sqlgg -gen none -dialect=tidb - <<'EOF'
  > CREATE TABLE t (c VARCHAR(10), d INT);
  > TRUNCATE TABLE t;
  > TRUNCATE t;
  > ALTER TABLE t MODIFY COLUMN c VARCHAR(10) COMMENT 'x';
  > ALTER TABLE t ADD COLUMN e INT COMMENT 'y';
  > ALTER TABLE t DROP PARTITION p0;
  > ALTER TABLE t TRUNCATE PARTITION p0;
  > ALTER TABLE t TRUNCATE PARTITION ALL;
  > ALTER TABLE t EXCHANGE PARTITION p0 WITH TABLE t2;
  > ALTER TABLE t REORGANIZE PARTITION p0 INTO (PARTITION p1 VALUES LESS THAN (10), PARTITION p2 VALUES LESS THAN MAXVALUE);
  > ALTER TABLE t REORGANIZE PARTITION p0 INTO (PARTITION p1 VALUES IN (1, 2));
  > ALTER TABLE t AUTO_INCREMENT = 100;
  > ALTER TABLE t AUTO_INCREMENT 100;
  > ALTER TABLE t FORCE AUTO_INCREMENT = 100;
  > ALTER TABLE t AUTO_RANDOM_BASE = 100;
  > ALTER TABLE t FORCE AUTO_RANDOM_BASE = 100;
  > ALTER TABLE t AUTO_ID_CACHE = 100;
  > ALTER TABLE t SHARD_ROW_ID_BITS = 4;
  > ALTER TABLE t ADD PARTITION (PARTITION p3 VALUES LESS THAN (30));
  > ALTER TABLE t COMMENT = 'x';
  > ALTER TABLE t COMMENT 'x';
  > ALTER TABLE t ADD INDEX i (c);
  > ALTER TABLE t ALTER INDEX i INVISIBLE;
  > ALTER TABLE t ALTER INDEX i VISIBLE;
  > ALTER TABLE t ADD INDEX i2 (d) INVISIBLE;
  > ALTER TABLE t ADD INDEX i3 (d) COMMENT 'x';
  > ALTER TABLE t CHARACTER SET = utf8mb4;
  > ALTER TABLE t DEFAULT CHARSET = utf8mb4 COLLATE = utf8mb4_bin;
  > ALTER TABLE t COLLATE = utf8mb4_bin;
  > ALTER TABLE t ADD COLUMN g TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP;
  > ALTER TABLE t ADD COLUMN h INT CHECK (h > 0);
  > ALTER TABLE t ADD COLUMN h2 INT CHECK (h2 > 0) NOT ENFORCED NOT NULL;
  > ALTER TABLE t ADD COLUMN h3 INT CHECK (h3 > 0) ENFORCED;
  > ALTER TABLE t ADD CHECK (d > 0) NOT ENFORCED;
  > ALTER TABLE t ADD COLUMN k INT AS (d + 1) VIRTUAL;
  > ALTER TABLE t ADD COLUMN l VARCHAR(10) NULL COLLATE utf8mb4_bin;
  > DROP INDEX i ON t;
  > DROP INDEX IF EXISTS i2 ON t;
  > ALTER TABLE t DROP KEY i3;
  > ALTER TABLE t DROP CONSTRAINT c1;
  > ALTER TABLE t DROP PARTITION p0, ADD COLUMN f INT;
  > ALTER TABLE t PARTITION BY HASH (d) PARTITIONS 4;
  > ALTER TABLE t PARTITION BY LINEAR HASH (d) PARTITIONS 4;
  > ALTER TABLE t PARTITION BY KEY (d) PARTITIONS 4;
  > ALTER TABLE t PARTITION BY LINEAR KEY ALGORITHM = 2 () PARTITIONS 4;
  > ALTER TABLE t PARTITION BY RANGE (d) (PARTITION p0 VALUES LESS THAN (10), PARTITION p1 VALUES LESS THAN MAXVALUE);
  > ALTER TABLE t PARTITION BY RANGE COLUMNS (c) (PARTITION p0 VALUES LESS THAN ('m'));
  > ALTER TABLE t PARTITION BY LIST (d) (PARTITION p0 VALUES IN (1, 2));
  > ALTER TABLE t PARTITION BY LIST COLUMNS (c) (PARTITION p0 VALUES IN ('a', 'b'));
  > ALTER TABLE t REMOVE PARTITIONING;
  > ALTER TABLE t SET TIFLASH REPLICA 2 LOCATION LABELS 'zone', 'rack';
  > ALTER TABLE t SET HYPO TIFLASH REPLICA 1;
  > ALTER TABLE t ALTER CHECK c1 NOT ENFORCED;
  > ALTER TABLE t ALTER CONSTRAINT c1 ENFORCED;
  > CREATE SEQUENCE IF NOT EXISTS s INCREMENT BY 2 START WITH 10 MINVALUE 1 MAXVALUE 1000 CACHE 20 CYCLE COMMENT = 'seq';
  > ALTER SEQUENCE IF EXISTS s RESTART WITH 50 INCREMENT = 5 NOCACHE NOCYCLE NOMAXVALUE NOMINVALUE;
  > DROP SEQUENCE IF EXISTS s;
  > RECOVER TABLE t;
  > DROP VIEW IF EXISTS t;
  > CREATE TABLE names (
  >   truncate INT, recover INT,
  >   less INT, than INT, exchange TEXT, reorganize INT, database_id INT, force INT, visible INT, invisible INT,
  >   partitioning INT, partitions INT, linear INT, hash INT, list INT, columns INT,
  >   tiflash INT, replica INT, location INT, labels INT, hypo INT,
  >   sequence INT, increment INT, start INT, minvalue INT, nominvalue INT, nomaxvalue INT,
  >   cycle INT, nocycle INT, restart INT, enforced INT
  > );
  > SELECT truncate, recover, less, than, exchange, reorganize, force, visible, invisible,
  >        partitioning, partitions, linear, hash, list, columns,
  >        tiflash, replica, location, labels, hypo,
  >        sequence, increment, start, minvalue, nominvalue, nomaxvalue,
  >        cycle, nocycle, restart, enforced
  > FROM names;
  > EOF

TRUNCATE(x, d) function call still parses when TRUNCATE is unreserved

  $ sqlgg ir - <<'EOF' | grep -o '"fn_name":"truncate"'
  > SELECT TRUNCATE(1.234, 2);
  > EOF
  W: unknown function "truncate" of 2 arguments, treating as untyped
  "fn_name":"truncate"

Standalone NOT ENFORCED without CHECK is rejected

  $ sqlgg -gen none -dialect=tidb - <<'EOF' 2>&1
  > CREATE TABLE t (c INT NOT ENFORCED);
  > EOF
  ==> CREATE TABLE t (c INT NOT ENFORCED)
  Position 1:34 Tokens: ENFORCED)
  Error: syntax error
  Errors encountered, no code generated
  [1]

Column comments are kept in the IR

  $ printf "ALTER TABLE t MODIFY COLUMN c VARCHAR(10) COMMENT 'x';\n" | sqlgg ir - | grep -o '"Comment","[^"]*"'
  "Comment","x"
  $ printf "CREATE TABLE t (c INT COMMENT 'z');\n" | sqlgg ir - | grep -o '"Comment","[^"]*"'
  "Comment","z"

Auto ID table options are IR actions

  $ sqlgg ir - <<'EOF' | grep -oE '"(AutoIncrement|AutoRandomBase)","(Rebase|Force_rebase)",[0-9]+|"(AutoIdCache|ShardRowIdBits)",[0-9]+'
  > ALTER TABLE t AUTO_INCREMENT = 100;
  > ALTER TABLE t FORCE AUTO_INCREMENT = 200;
  > ALTER TABLE t AUTO_RANDOM_BASE = 300;
  > ALTER TABLE t FORCE AUTO_RANDOM_BASE = 400;
  > ALTER TABLE t AUTO_ID_CACHE = 500;
  > ALTER TABLE t SHARD_ROW_ID_BITS = 4;
  > EOF
  "AutoIncrement","Rebase",100
  "AutoIncrement","Force_rebase",200
  "AutoRandomBase","Rebase",300
  "AutoRandomBase","Force_rebase",400
  "AutoIdCache",500
  "ShardRowIdBits",4

Column options are kept in the IR

  $ sqlgg ir - <<'EOF' | grep -oE '"(On_update|Check|Generated|Collate)"(,"[^"]*")?'
  > ALTER TABLE t ADD COLUMN g TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP;
  > ALTER TABLE t ADD COLUMN h INT CHECK (h > 0);
  > ALTER TABLE t ADD COLUMN k INT AS (d + 1) VIRTUAL;
  > ALTER TABLE t ADD COLUMN l VARCHAR(10) NULL COLLATE utf8mb4_bin;
  > EOF
  "On_update"
  "Check"
  "Generated"
  "Collate","utf8mb4_bin"

Table options, partitions and index visibility are IR actions

  $ sqlgg ir - <<'EOF' | grep -oE '\["(AddPartition|TableComment|TableCollation|AlterIndexVisibility|Default_or_convert_to)"|"(Visible|Invisible)"'
  > ALTER TABLE t ADD PARTITION (PARTITION p3 VALUES LESS THAN (30));
  > ALTER TABLE t COMMENT = 'x';
  > ALTER TABLE t COLLATE = utf8mb4_bin;
  > ALTER TABLE t CHARACTER SET = utf8mb4;
  > ALTER TABLE t ALTER INDEX i INVISIBLE;
  > ALTER TABLE t ALTER INDEX i VISIBLE;
  > EOF
  ["AddPartition"
  ["TableComment"
  ["TableCollation"
  ["Default_or_convert_to"
  ["AlterIndexVisibility"
  "Invisible"
  ["AlterIndexVisibility"
  "Visible"

DROP VIEW and DROP INDEX are distinguished from DROP TABLE

  $ sqlgg ir - <<'EOF' | grep -oE '"stmt":\["[A-Za-z]+"|"DropIndex","[^"]*"'
  > DROP TABLE t;
  > DROP VIEW v;
  > DROP INDEX i ON t;
  > EOF
  "stmt":["Drop"
  "stmt":["DropView"
  "stmt":["Alter"
  "DropIndex","i"

TiDB-only auto ID options are rejected under other dialects

  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (c INT);
  > ALTER TABLE t AUTO_INCREMENT = 100;
  > EOF
  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (c INT);
  > ALTER TABLE t FORCE AUTO_INCREMENT = 100;
  > EOF
  Feature RebaseAutoId is not supported for dialect MySQL (supported by: TiDB) at 
  Errors encountered, no code generated
  [1]
  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (c INT);
  > ALTER TABLE t AUTO_ID_CACHE = 100;
  > EOF
  Feature AutoIdCache is not supported for dialect MySQL (supported by: TiDB) at 
  Errors encountered, no code generated
  [1]
  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (c INT);
  > ALTER TABLE t AUTO_RANDOM_BASE = 100;
  > EOF
  Feature RebaseAutoId is not supported for dialect MySQL (supported by: TiDB) at 
  Errors encountered, no code generated
  [1]

Database statements are IR statements

  $ sqlgg ir - <<'EOF' | grep -oE '"(CreateDatabase|AlterDatabase|DropDatabase|Database_[a-z_]+)"'
  > CREATE DATABASE IF NOT EXISTS d DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_bin;
  > ALTER DATABASE d CHARACTER SET utf8mb4;
  > ALTER DATABASE d COLLATE utf8mb4_bin;
  > ALTER DATABASE d DEFAULT CHARACTER SET = utf8mb4;
  > DROP DATABASE IF EXISTS d;
  > EOF
  "CreateDatabase"
  "Database_charset_and_collation"
  "AlterDatabase"
  "Database_charset"
  "AlterDatabase"
  "Database_collation"
  "AlterDatabase"
  "Database_charset"
  "DropDatabase"

IR parsing of CREATE TABLE ... LIKE does not resolve the source table

  $ sqlgg ir - <<'EOF'
  > CREATE TABLE t2 LIKE t;
  > EOF
  {"statements":[{"sql":"CREATE TABLE t2 LIKE t","props":[],"resolution":["Parsed",{"stmt":["Create",{"value":{"db":null,"tn":"t2"},"pos":[13,15]},["Like",{"db":null,"tn":"t"}]],"dialect_features":[{"feature":"CreateTableLike","pos":[0,0],"state":{"supported":["MySQL","TiDB"],"unsupported":["PostgreSQL","SQLite"],"unknown":[]}}]}]}]}

  $ sqlgg -gen none - <<'EOF' 2>&1
  > CREATE TABLE t (id INT);
  > CREATE TABLE t2 LIKE t;
  > SELECT id FROM t2;
  > EOF

FOREIGN KEY and CHECK can be added without CONSTRAINT

  $ sqlgg ir - <<'EOF' | grep -oE '\["AddConstraint",null,\["(Foreign|Check)"'
  > ALTER TABLE t ADD FOREIGN KEY (b) REFERENCES p (id) ON DELETE CASCADE;
  > ALTER TABLE t ADD CHECK (a > 0);
  > EOF
  ["AddConstraint",null,["Foreign"
  ["AddConstraint",null,["Check"

Partitioning, TiFlash, sequence and check enforcement actions in the IR

  $ sqlgg ir - <<'EOF' | grep -oE '"(PartitionBy|Partition_[a-z_]+|RemovePartitioning|SetTiflashReplica|Tiflash_replica|Hypo_tiflash_replica|AlterCheck|Enforced|Not_enforced|CreateSequence|AlterSequence|DropSequence|Alter_db_tiflash_replica)"'
  > ALTER TABLE t PARTITION BY HASH (a) PARTITIONS 4;
  > ALTER TABLE t PARTITION BY RANGE COLUMNS (a) (PARTITION p0 VALUES LESS THAN (10));
  > ALTER TABLE t REMOVE PARTITIONING;
  > ALTER TABLE t SET TIFLASH REPLICA 2 LOCATION LABELS 'z1', 'z2';
  > ALTER TABLE t SET HYPO TIFLASH REPLICA 1;
  > ALTER DATABASE d SET TIFLASH REPLICA 1;
  > ALTER TABLE t ALTER CHECK chk NOT ENFORCED;
  > CREATE SEQUENCE s INCREMENT BY 1 START WITH 1;
  > ALTER SEQUENCE s RESTART WITH 10;
  > DROP SEQUENCE s;
  > EOF
  "PartitionBy"
  "Partition_hash"
  "PartitionBy"
  "Partition_range_columns"
  "RemovePartitioning"
  "SetTiflashReplica"
  "Tiflash_replica"
  "SetTiflashReplica"
  "Hypo_tiflash_replica"
  "Alter_db_tiflash_replica"
  "AlterCheck"
  "Not_enforced"
  "CreateSequence"
  "AlterSequence"
  "DropSequence"

The bdr_non_replicable flag is a statement property in the IR

  $ sqlgg ir - <<'EOF' | grep -o '"Bdr_non_replicable"'
  > -- [sqlgg] id=20260101000000_truncate_t
  > -- [sqlgg] bdr_non_replicable
  > TRUNCATE TABLE t;
  > EOF
  "Bdr_non_replicable"

TiDB-only statements are rejected under other dialects

  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > RECOVER TABLE t;
  > EOF
  Feature RecoverTable is not supported for dialect MySQL (supported by: TiDB) at 
  Errors encountered, no code generated
  [1]
  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE t (c INT);
  > ALTER TABLE t DROP PARTITION p0;
  > EOF
  Feature PartitionOperations is not supported for dialect PostgreSQL (supported by: MySQL, TiDB) at 
  Errors encountered, no code generated
  [1]
  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT);
  > CREATE TABLE t2 LIKE t;
  > EOF
  Feature CreateTableLike is not supported for dialect PostgreSQL (supported by: MySQL, TiDB) at 
  Errors encountered, no code generated
  [1]
  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE t (c VARCHAR(10) NULL COLLATE utf8mb4_bin);
  > EOF
  Feature Collation is not supported for dialect PostgreSQL (supported by: MySQL, TiDB) at COLLATE utf8mb4_bin
  Errors encountered, no code generated
  [1]
  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE t (c INT);
  > CREATE INDEX idx_c ON t (c) INVISIBLE;
  > EOF
  Feature IndexVisibility is not supported for dialect PostgreSQL (supported by: MySQL, TiDB) at INVISIBLE
  Errors encountered, no code generated
  [1]
  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE t (c INT, INDEX idx_c (c) COMMENT 'x');
  > EOF
  Feature IndexComment is not supported for dialect PostgreSQL (supported by: MySQL, TiDB) at COMMENT 'x'
  Errors encountered, no code generated
  [1]
  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE DATABASE d;
  > DROP DATABASE d;
  > CREATE SEQUENCE s START WITH 1;
  > DROP SEQUENCE s;
  > CREATE TABLE chk_pg (x INT CHECK (x > 0) NOT NULL, CHECK (x < 10));
  > EOF
  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE t (x INT CHECK (x > 0) NOT ENFORCED);
  > EOF
  Feature EnforcedCheck is not supported for dialect PostgreSQL (supported by: MySQL, TiDB) at NOT ENFORCED
  Errors encountered, no code generated
  [1]
  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE t (x INT, CONSTRAINT c CHECK (x > 0) ENFORCED);
  > EOF
  Feature EnforcedCheck is not supported for dialect PostgreSQL (supported by: MySQL, TiDB) at ENFORCED
  Errors encountered, no code generated
  [1]
  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE t (x INT);
  > ALTER TABLE t ADD CHECK (x > 0) NOT ENFORCED;
  > EOF
  Feature EnforcedCheck is not supported for dialect PostgreSQL (supported by: MySQL, TiDB) at NOT ENFORCED
  Errors encountered, no code generated
  [1]
  $ sqlgg -gen none -dialect=sqlite - <<'EOF' 2>&1
  > CREATE DATABASE d;
  > EOF
  Feature DatabaseDdl is not supported for dialect SQLite (supported by: MySQL, TiDB, PostgreSQL) at 
  Errors encountered, no code generated
  [1]
  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE DATABASE d CHARACTER SET utf8mb4;
  > EOF
  Feature DatabaseCharsetAndCollation is not supported for dialect PostgreSQL (supported by: MySQL, TiDB) at 
  Errors encountered, no code generated
  [1]
  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (c INT);
  > ALTER TABLE t SET TIFLASH REPLICA 1;
  > EOF
  Feature TiflashReplica is not supported for dialect MySQL (supported by: TiDB) at 
  Errors encountered, no code generated
  [1]
  $ sqlgg -gen none -dialect=mysql - <<'EOF' 2>&1
  > CREATE SEQUENCE s;
  > EOF
  Feature SequenceDdl is not supported for dialect MySQL (supported by: PostgreSQL, TiDB) at 
  Errors encountered, no code generated
  [1]
  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE t (c INT);
  > RENAME TABLE t TO t2;
  > EOF
  Feature RenameTable is not supported for dialect PostgreSQL (supported by: MySQL, TiDB) at 
  Errors encountered, no code generated
  [1]

TRUNCATE and RECOVER TABLE are IR statements

  $ sqlgg ir - <<'EOF'
  > TRUNCATE TABLE t;
  > RECOVER TABLE t;
  > EOF
  {"statements":[{"sql":"TRUNCATE TABLE t","props":[],"resolution":["Parsed",{"stmt":["Truncate",{"db":null,"tn":"t"}],"dialect_features":[{"feature":"TruncateTable","pos":[0,0],"state":{"supported":["MySQL","TiDB","PostgreSQL"],"unsupported":["SQLite"],"unknown":[]}}]}]},{"sql":"RECOVER TABLE t","props":[],"resolution":["Parsed",{"stmt":["RecoverTable",{"db":null,"tn":"t"}],"dialect_features":[{"feature":"RecoverTable","pos":[0,0],"state":{"supported":["TiDB"],"unsupported":["MySQL","PostgreSQL","SQLite"],"unknown":[]}}]}]}]}
