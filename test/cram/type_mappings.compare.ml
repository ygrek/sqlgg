module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking

  let create_table_37 db  =
    T.execute db ("CREATE TABLE table_37 (\n\
                col_1 INT PRIMARY KEY,\n\
                        col_2 INT NOT NULL\n\
      )") T.no_params

  let create_table_38 db  =
    T.execute db ("CREATE TABLE table_38 (\n\
                col_3 INT PRIMARY KEY,\n\
        col_4 TEXT NOT NULL\n\
      )") T.no_params

  let select_2 db  callback =
    let invoke_callback stmt =
      callback
        ~deeply_nested_query:(HelloWorld.get_column_nullable (T.get_column_int64_nullable stmt 0))
        ~col_2:(Abcdefg.test (T.get_column_int64 stmt 1))
        ~col_3:(FooBar.get_column_nullable (T.get_column_int64_nullable stmt 2))
        ~col_4:(T.get_column_Text_nullable stmt 3)
    in
    T.select db ("SELECT \n\
  (\n\
    SELECT MAX(x.col_val)\n\
    FROM (\n\
      SELECT col_1 as col_val,\n\
           1 + 3 as aaaaa,\n\
           1 + col_1 as bbb\n\
      FROM table_37 \n\
      WHERE col_1 > (\n\
        SELECT MIN(col_1) \n\
        FROM table_37\n\
      )\n\
    ) as x\n\
  ) as deeply_nested_query,\n\
  col_2,\n\
  col_3,\n\
  col_4\n\
FROM table_37\n\
LEFT JOIN table_38\n\
  ON table_37.col_1 = table_38.col_3") T.no_params invoke_callback

  module Fold = struct
    let select_2 db  callback acc =
      let invoke_callback stmt =
        callback
          ~deeply_nested_query:(HelloWorld.get_column_nullable (T.get_column_int64_nullable stmt 0))
          ~col_2:(Abcdefg.test (T.get_column_int64 stmt 1))
          ~col_3:(FooBar.get_column_nullable (T.get_column_int64_nullable stmt 2))
          ~col_4:(T.get_column_Text_nullable stmt 3)
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("SELECT \n\
  (\n\
    SELECT MAX(x.col_val)\n\
    FROM (\n\
      SELECT col_1 as col_val,\n\
           1 + 3 as aaaaa,\n\
           1 + col_1 as bbb\n\
      FROM table_37 \n\
      WHERE col_1 > (\n\
        SELECT MIN(col_1) \n\
        FROM table_37\n\
      )\n\
    ) as x\n\
  ) as deeply_nested_query,\n\
  col_2,\n\
  col_3,\n\
  col_4\n\
FROM table_37\n\
LEFT JOIN table_38\n\
  ON table_37.col_1 = table_38.col_3") T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

  end (* module Fold *)
  
  module List = struct
    let select_2 db  callback =
      let invoke_callback stmt =
        callback
          ~deeply_nested_query:(HelloWorld.get_column_nullable (T.get_column_int64_nullable stmt 0))
          ~col_2:(Abcdefg.test (T.get_column_int64 stmt 1))
          ~col_3:(FooBar.get_column_nullable (T.get_column_int64_nullable stmt 2))
          ~col_4:(T.get_column_Text_nullable stmt 3)
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("SELECT \n\
  (\n\
    SELECT MAX(x.col_val)\n\
    FROM (\n\
      SELECT col_1 as col_val,\n\
           1 + 3 as aaaaa,\n\
           1 + col_1 as bbb\n\
      FROM table_37 \n\
      WHERE col_1 > (\n\
        SELECT MIN(col_1) \n\
        FROM table_37\n\
      )\n\
    ) as x\n\
  ) as deeply_nested_query,\n\
  col_2,\n\
  col_3,\n\
  col_4\n\
FROM table_37\n\
LEFT JOIN table_38\n\
  ON table_37.col_1 = table_38.col_3") T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

  end (* module List *)
end (* module Sqlgg *)
