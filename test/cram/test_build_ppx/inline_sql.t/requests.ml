module Wrapped_id = struct
  type t = Id of int64

  let set_param (Id value) = value
  let get_column value = Id value
  let get_column_nullable value = Option.map get_column value
end

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

let users_cte =
  [%sql.Request {| WITH x AS (SELECT id, name FROM users) SELECT id, name FROM x WHERE id = @id |}]

let users_in_sub =
  [%sql.Request {| SELECT id, name FROM users WHERE id IN (SELECT id FROM users WHERE id = @id) |}]

let untyped = [%sql.Request {| SELECT id, NULL AS extra FROM users WHERE @cond |}]

let wrapped_user =
  [%sql.Request {| SELECT id, parent_id FROM wrapped_users WHERE id = @id |}]

let wrapped_by_ids =
  [%sql.Request {| SELECT id FROM wrapped_users WHERE id IN @ids |}]

let insert_wrapped =
  [%sql.Request {| INSERT INTO wrapped_users (id, parent_id) VALUES @rows |}]
