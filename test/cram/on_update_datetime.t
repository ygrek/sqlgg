MySQL ON UPDATE <datetime> column attribute is accepted in column definitions
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

ON UPDATE still works as a foreign key referential action
  $ sqlgg -gen none -dialect=mysql - <<'EOF'
  > CREATE TABLE p (id INT PRIMARY KEY);
  > CREATE TABLE c (
  >   id INT PRIMARY KEY,
  >   p_id INT NOT NULL,
  >   updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  >   FOREIGN KEY (p_id) REFERENCES p (id) ON UPDATE CASCADE ON DELETE RESTRICT
  > );
  > EOF
