let find_user =
  [%sql.Request {| SELECT id, name FROM users WHERE id = @id |}]

let rename_user =
  [%sql.Request {| UPDATE users SET name = @name WHERE id = @id |}]

let insert_wide =
  [%sql.Request {| INSERT INTO wide (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12) VALUES (@c1, @c2, @c3, @c4, @c5, @c6, @c7, @c8, @c9, @c10, @c11, @c12) |}]

let find_wide =
  [%sql.Request {| SELECT c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12 FROM wide WHERE c1 = @c1 |}]

let post_state = [%sql.Request {| SELECT state FROM posts WHERE id = @id |}]

let publish = [%sql.Request {| UPDATE posts SET state = @state WHERE id = @id |}]

let users_by_ids = [%sql.Request {| SELECT id, name FROM users WHERE id IN @ids |}]

let insert_users = [%sql.Request {| INSERT INTO users (id, name, email) VALUES @rows |}]

let find_by =
  [%sql.Request {| SELECT id FROM users WHERE @filter { By_id { id = @id } | By_name { name = @name } } |}]

let find_after =
  [%sql.Request {| SELECT id FROM users WHERE id > @min AND @name_filter { Some { name = @name } | None { TRUE } } |}]
