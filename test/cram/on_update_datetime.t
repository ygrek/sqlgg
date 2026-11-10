MySQL accepts column ON UPDATE CURRENT_TIMESTAMP
  $ sqlgg -gen none -dialect=mysql - <<'EOF'
  > CREATE TABLE t (
  >   id INT PRIMARY KEY,
  >   created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  >   updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  >   u2 DATETIME DEFAULT NOW() ON UPDATE NOW(),
  >   u3 DATETIME(6) DEFAULT CURRENT_TIMESTAMP(6) ON UPDATE CURRENT_TIMESTAMP(6),
  >   u4 TIMESTAMP ON UPDATE CURRENT_TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
  > );
  > SELECT id, updated_at FROM t WHERE updated_at > @since;
  > EOF

Foreign key ON UPDATE CASCADE parses
  $ sqlgg -gen none -dialect=mysql - <<'EOF'
  > CREATE TABLE p (id INT PRIMARY KEY);
  > CREATE TABLE c (
  >   id INT PRIMARY KEY,
  >   p_id INT NOT NULL,
  >   updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  >   FOREIGN KEY (p_id) REFERENCES p (id) ON UPDATE CASCADE ON DELETE RESTRICT
  > );
  > EOF

TiDB accepts column ON UPDATE CURRENT_TIMESTAMP
  $ sqlgg -gen none -dialect=tidb - <<'EOF'
  > CREATE TABLE t (updated_at TIMESTAMP ON UPDATE CURRENT_TIMESTAMP);
  > EOF

PostgreSQL rejects column ON UPDATE CURRENT_TIMESTAMP
  $ sqlgg -gen none -dialect=postgresql - <<'EOF' 2>&1
  > CREATE TABLE t (updated_at TIMESTAMP ON UPDATE CURRENT_TIMESTAMP);
  > EOF
  Feature OnUpdateTimestamp is not supported for dialect PostgreSQL (supported by: MySQL, TiDB) at ON UPDATE CURRENT_TIMESTAMP
  Errors encountered, no code generated
  [1]

-no-check on_update_timestamp turns error into warning
  $ sqlgg -gen none -dialect=postgresql -no-check on_update_timestamp - <<'EOF' 2>&1
  > CREATE TABLE t (updated_at TIMESTAMP ON UPDATE CURRENT_TIMESTAMP);
  > EOF
  Warning: Feature OnUpdateTimestamp is not supported for dialect PostgreSQL, proceeding anyway at ON UPDATE CURRENT_TIMESTAMP
