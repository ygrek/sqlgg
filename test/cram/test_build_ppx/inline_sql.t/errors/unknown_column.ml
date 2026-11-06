let nicknames =
  [%sql
    {|
      SELECT id, nick
      FROM users
    |}]
