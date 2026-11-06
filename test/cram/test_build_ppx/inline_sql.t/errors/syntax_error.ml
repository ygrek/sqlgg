let broken =
  [%sql
    {|
      -- @broken
      SELECT id
      FROM users /* a comment shifts nothing */ WHERE id = = @id
    |}]
