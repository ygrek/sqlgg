let clear_email = [%sql {| UPDATE users SET email = NULL |}]
