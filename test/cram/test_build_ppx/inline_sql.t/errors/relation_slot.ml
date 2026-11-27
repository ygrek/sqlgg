let count = [%sql {| SELECT COUNT(1) FROM @source AS s |}]
