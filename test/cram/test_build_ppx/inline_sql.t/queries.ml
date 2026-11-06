type user = {
  id : int64;
  name : string option;
}
[@@deriving sqlgg]

module Make (T : Sqlgg_traits.M_io) = struct
  let find_user =
    [%sql
      {|
        -- @find_user
        SELECT id, name
        FROM users
        WHERE id = @id
      |}]

  let add_user =
    [%sql
      {|
        -- @add_user
        INSERT INTO users (id, name, email) VALUES (@id, @name, @email)
      |}]

  let add_users =
    [%sql {| INSERT INTO users (id, name, email) VALUES @rows |}]

  let rename_user =
    [%sql
      {|
        -- @rename_user
        UPDATE users SET name = @name, nickname = @nickname WHERE id = @id
      |}]

  let titles_by =
    [%sql
      {|
        -- @titles_by
        SELECT p.title, u.nickname
        FROM posts p
        JOIN users u ON u.id = p.author_id
        WHERE p.author_id = @author AND p.state = @state
      |}]

  let publish =
    [%sql
      {|
        -- @publish
        UPDATE posts SET state = @state WHERE id = @id
      |}]
end
