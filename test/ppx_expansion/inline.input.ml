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

  let dynamic = [%sql {| -- [sqlgg] dynamic_select=true
    SELECT id FROM users |}]

  let bad_property = [%sql {| -- [sqlgg] colour=red
    SELECT id FROM users |}]

  let empty = [%sql {| -- nothing but a comment |}]

  let two_literals = [%sql "SELECT 1" "SELECT 2"]
end
