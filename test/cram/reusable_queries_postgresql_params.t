PostgreSQL parameter numbering in reusable queries, distinct parameters:

  $ sqlgg -no-header -gen caml -params postgresql -dialect postgresql - <<'EOF' 2>/dev/null | awk '/^  let get_users/{p=1} p&&/^$/{exit} p{print}'
  > CREATE TABLE person (
  >     id INT PRIMARY KEY,
  >     user_id INT NOT NULL,
  >     name TEXT NOT NULL
  > );
  > CREATE TABLE "user" (
  >     id INT PRIMARY KEY,
  >     username TEXT NOT NULL
  > );
  > -- @get_persons | include: reuse
  > SELECT *
  > FROM person
  > WHERE name LIKE @name;
  > -- @get_users
  > WITH person AS &get_persons
  > SELECT *
  > FROM "user"
  > JOIN person ON person.user_id = "user".id
  > WHERE "user".id > @min_id;
  > EOF
    let get_users db ~name ~min_id callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~username:(T.get_column_Text stmt 1)
          ~id0:(T.get_column_Int stmt 2)
          ~user_id:(T.get_column_Int stmt 3)
          ~name:(T.get_column_Text stmt 4)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_Text p name;
        T.set_param_Int p min_id;
        T.finish_params p
      in
      T.select db ("WITH person AS (SELECT *\n\
  FROM person\n\
  WHERE name LIKE $1)\n\
  SELECT *\n\
  FROM \"user\"\n\
  JOIN person ON person.user_id = \"user\".id\n\
  WHERE \"user\".id > $1") set_params invoke_callback

PostgreSQL parameter numbering in reusable queries, shared parameter:

  $ sqlgg -no-header -gen caml -params postgresql -dialect postgresql - <<'EOF' 2>/dev/null | awk '/^  let get_users/{p=1} p&&/^$/{exit} p{print}'
  > CREATE TABLE person (
  >     id INT PRIMARY KEY,
  >     user_id INT NOT NULL,
  >     name TEXT NOT NULL
  > );
  > CREATE TABLE "user" (
  >     id INT PRIMARY KEY,
  >     username TEXT NOT NULL
  > );
  > -- @get_persons | include: reuse
  > SELECT *
  > FROM person
  > WHERE
  >   name LIKE @name
  >   AND person.id > @min_id;
  > -- @get_users
  > WITH person AS &get_persons
  > SELECT *
  > FROM "user"
  > JOIN person ON person.user_id = "user".id
  > WHERE
  >   username LIKE @username
  >   AND "user".id > @min_id;
  > EOF
    let get_users db ~name ~username ~min_id callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~username:(T.get_column_Text stmt 1)
          ~id0:(T.get_column_Int stmt 2)
          ~user_id:(T.get_column_Int stmt 3)
          ~name:(T.get_column_Text stmt 4)
      in
      let set_params stmt =
        let p = T.start_params stmt (4) in
        T.set_param_Text p name;
        T.set_param_Int p min_id;
        T.set_param_Text p username;
        T.set_param_Int p min_id;
        T.finish_params p
      in
      T.select db ("WITH person AS (SELECT *\n\
  FROM person\n\
  WHERE\n\
    name LIKE $1\n\
    AND person.id > $2)\n\
  SELECT *\n\
  FROM \"user\"\n\
  JOIN person ON person.user_id = \"user\".id\n\
  WHERE\n\
    username LIKE $1\n\
    AND \"user\".id > $2") set_params invoke_callback

PostgreSQL parameter numbering with two reusable queries in one outer query:

  $ sqlgg -no-header -gen caml -params postgresql -dialect postgresql - <<'EOF' 2>/dev/null | awk '/^  let two_ctes/{p=1} p&&/^$/{exit} p{print}'
  > CREATE TABLE person (
  >     id INT PRIMARY KEY,
  >     user_id INT NOT NULL,
  >     name TEXT NOT NULL
  > );
  > CREATE TABLE "user" (
  >     id INT PRIMARY KEY,
  >     username TEXT NOT NULL
  > );
  > -- @get_persons | include: reuse
  > SELECT * FROM person WHERE name LIKE @name;
  > -- @get_admins | include: reuse
  > SELECT * FROM "user" WHERE username LIKE @admin;
  > -- @two_ctes
  > WITH p AS &get_persons, a AS &get_admins
  > SELECT p.id, a.username
  > FROM p JOIN a ON a.id = p.user_id
  > WHERE p.id > @min_id;
  > EOF
    let two_ctes db ~name ~admin ~min_id callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~username:(T.get_column_Text stmt 1)
      in
      let set_params stmt =
        let p = T.start_params stmt (3) in
        T.set_param_Text p name;
        T.set_param_Text p admin;
        T.set_param_Int p min_id;
        T.finish_params p
      in
      T.select db ("WITH p AS (SELECT * FROM person WHERE name LIKE $1), a AS (SELECT * FROM \"user\" WHERE username LIKE $1)\n\
  SELECT p.id, a.username\n\
  FROM p JOIN a ON a.id = p.user_id\n\
  WHERE p.id > $1") set_params invoke_callback

PostgreSQL parameter numbering with reusable queries threaded into each other:

  $ sqlgg -no-header -gen caml -params postgresql -dialect postgresql - <<'EOF' 2>/dev/null | awk '/^  let outer_q/{p=1} p&&/^$/{exit} p{print}'
  > CREATE TABLE person (
  >     id INT PRIMARY KEY,
  >     user_id INT NOT NULL,
  >     name TEXT NOT NULL
  > );
  > -- @get_persons | include: reuse
  > SELECT * FROM person WHERE name LIKE @name;
  > -- @wrap_persons | include: reuse
  > WITH inner_p AS &get_persons
  > SELECT * FROM inner_p WHERE inner_p.id > @inner_min;
  > -- @outer_q
  > WITH p AS &wrap_persons
  > SELECT * FROM p WHERE p.user_id > @outer_min;
  > EOF
    let outer_q db ~name ~inner_min ~outer_min callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~user_id:(T.get_column_Int stmt 1)
          ~name:(T.get_column_Text stmt 2)
      in
      let set_params stmt =
        let p = T.start_params stmt (3) in
        T.set_param_Text p name;
        T.set_param_Int p inner_min;
        T.set_param_Int p outer_min;
        T.finish_params p
      in
      T.select db ("WITH p AS (WITH inner_p AS (SELECT * FROM person WHERE name LIKE $1)\n\
  SELECT * FROM inner_p WHERE inner_p.id > $1)\n\
  SELECT * FROM p WHERE p.user_id > $1") set_params invoke_callback
