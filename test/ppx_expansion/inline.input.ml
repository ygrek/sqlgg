module Make (T : Sqlgg_traits.M_io) = struct
  let find_user = [%sql {| SELECT id, name FROM users WHERE id = @id |}]

  let rename = [%sql {| -- @rename_user
    UPDATE users SET name = @name WHERE id = @id |}]

  let add_user = [%sql "INSERT INTO users (id, name) VALUES (@id, @name)"]

  let seen = [%sql {| UPDATE users SET seen_at = @seen_at |}]

  let noted = [%sql {| UPDATE audit SET noted_at = @noted_at |}]

  let by_state = [%sql {| SELECT id FROM posts WHERE state = @state |}]

  let set_state = [%sql {| UPDATE posts SET state = @state WHERE id = @id |}]

  let create_leaked = [%sql {| CREATE TABLE leaked (x INT) |}]

  let from_leaked = [%sql {| SELECT x FROM leaked |}]

  let create_function =
    [%sql {| CREATE FUNCTION leaked_fn(arg INTEGER) RETURNS INTEGER AS 'body' LANGUAGE sql |}]

  let call_function = [%sql {| SELECT leaked_fn(1) AS n FROM users |}]

  let substituted = [%sql {| -- [sqlgg] subst=tbl
    SELECT id FROM %%tbl%% |}]

  let verbatim = [%sql {| -- [sqlgg] noparse
    SELECT id FROM users |}]

  let reusable = [%sql {| -- [sqlgg] include=reuse
    SELECT id FROM users |}]

  let executable = [%sql {| -- [sqlgg] include=execute
    SELECT id FROM users |}]

  let dynamic = [%sql {| -- [sqlgg] dynamic_select=true
    SELECT id FROM users |}]

  let static = [%sql {| -- [sqlgg] dynamic_select=false
    SELECT id FROM users |}]

  let migration = [%sql {| -- [sqlgg] id=one
    SELECT id FROM users |}]

  let bad_property = [%sql {| -- [sqlgg] colour=red
    SELECT id FROM users |}]

  let empty = [%sql {| -- nothing but a comment |}]

  let two_literals = [%sql "SELECT 1" "SELECT 2"]
end

module With_database (Database : Sqlgg_traits.M_io) = struct
  let find_user =
    [%sql.Database {| SELECT id, name FROM users WHERE id = @id |}]
end

module With_nested_database
    (Runtime : sig module Database : Sqlgg_traits.M_io end) =
struct
  let find_user =
    [%sql.Runtime.Database {| SELECT id, name FROM users WHERE id = @id |}]
end

let find_user_request =
  [%sql.Request {| SELECT id, name FROM users WHERE id = @id |}]

let rename_user_request =
  [%sql.Request {| UPDATE users SET name = @name WHERE id = @id |}]

let in_list_request =
  [%sql.Request {| SELECT id FROM users WHERE id IN @ids |}]

let value_rows_request =
  [%sql.Request {| INSERT INTO users (id, name) VALUES @rows |}]

let tuple_in_request =
  [%sql.Request {| SELECT id FROM users WHERE (id, name) IN @pairs |}]

let choice_request =
  [%sql.Request {| SELECT id FROM users WHERE @filter { By_id { id = @id } | By_name { name = @name } } |}]

let option_request =
  [%sql.Request {| SELECT id FROM users WHERE id > @min AND @name_filter { Some { name = @name } | None { TRUE } } |}]

let count_users_request = [%sql.Request {| SELECT COUNT(*) FROM users |}]

let all_ids_request = [%sql.Request {| SELECT id FROM users |}]

let datetime_request = [%sql.Request {| SELECT seen_at FROM users |}]

let state_request = [%sql.Request {| SELECT state FROM posts WHERE id = @id |}]

let json_path_request =
  [%sql.Request {| SELECT id FROM docs WHERE JSON_CONTAINS_PATH(body, @mode, @path) |}]

let three_columns_request =
  [%sql.Request {| SELECT id, name, id AS again FROM users WHERE id = @id |}]
