let both = [%sql {| SELECT id FROM users; SELECT name FROM users |}]
