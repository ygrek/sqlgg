module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking

  let create_people db  =
    T.execute db ("CREATE TABLE people (\n\
  id INT AUTO_INCREMENT PRIMARY KEY,\n\
  data JSON\n\
)") T.no_params

  let create_people_data_json_kind_never_null_but_col_nullable db  =
    T.execute db ("CREATE TABLE people_data_json_kind_never_null_but_col_nullable (\n\
  id INT AUTO_INCREMENT PRIMARY KEY,\n\
  data JSON\n\
)") T.no_params

  let create_people_data_json_kind_never_null_and_col_strict db  =
    T.execute db ("CREATE TABLE people_data_json_kind_never_null_and_col_strict (\n\
  id INT AUTO_INCREMENT PRIMARY KEY,\n\
  data JSON NOT NULL\n\
)") T.no_params

  let one db  =
    T.execute db ("INSERT INTO people (data) VALUES\n\
  (JSON_OBJECT(\n\
    'name', 'Alice',\n\
    'age', 30,\n\
    'skills', JSON_ARRAY('SQL', 'Python'),\n\
    'address', JSON_OBJECT('city', 'Paris', 'zip', '75000')\n\
  )),\n\
  (JSON_OBJECT(\n\
    'name', 'Bob',\n\
    'age', 25,\n\
    'skills', JSON_ARRAY('JavaScript'),\n\
    'address', JSON_OBJECT('city', 'Berlin', 'zip', '10115')\n\
  )),\n\
  (JSON_OBJECT(\n\
    'name', 'Charlie',\n\
    'age', 35,\n\
    'skills', JSON_ARRAY(),\n\
    'address', JSON_OBJECT('city', 'New York', 'zip', '10001')\n\
  )),\n\
  (JSON_OBJECT(\n\
    'name', NULL,\n\
    'age', NULL\n\
  ))") T.no_params

  let two db  callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
        ~name:(T.get_column_Json_nullable stmt 1)
    in
    T.select db ("SELECT id, JSON_EXTRACT(data, '$.name') AS name\n\
FROM people\n\
WHERE JSON_UNQUOTE(JSON_EXTRACT(data, '$.address.city')) = 'Paris'") T.no_params invoke_callback

  let three db  callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
        ~data_with_active:(T.get_column_Json_nullable stmt 1)
    in
    T.select db ("SELECT\n\
  id,\n\
  JSON_SET(data, '$.active', TRUE) AS data_with_active\n\
FROM people") T.no_params invoke_callback

  let four db  callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
        ~data_with_active:(T.get_column_Json_nullable stmt 1)
    in
    T.select db ("SELECT\n\
  id,\n\
  JSON_SET(data, '$.active', TRUE) AS data_with_active\n\
FROM people_data_json_kind_never_null_but_col_nullable") T.no_params invoke_callback

  let five db  callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
        ~data_with_active:(T.get_column_Json stmt 1)
    in
    T.select db ("SELECT\n\
  id,\n\
  JSON_SET(data, '$.active', TRUE) AS data_with_active\n\
FROM people_data_json_kind_never_null_and_col_strict") T.no_params invoke_callback

  module Fold = struct
    let two db  callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name:(T.get_column_Json_nullable stmt 1)
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("SELECT id, JSON_EXTRACT(data, '$.name') AS name\n\
FROM people\n\
WHERE JSON_UNQUOTE(JSON_EXTRACT(data, '$.address.city')) = 'Paris'") T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let three db  callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~data_with_active:(T.get_column_Json_nullable stmt 1)
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("SELECT\n\
  id,\n\
  JSON_SET(data, '$.active', TRUE) AS data_with_active\n\
FROM people") T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let four db  callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~data_with_active:(T.get_column_Json_nullable stmt 1)
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("SELECT\n\
  id,\n\
  JSON_SET(data, '$.active', TRUE) AS data_with_active\n\
FROM people_data_json_kind_never_null_but_col_nullable") T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let five db  callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~data_with_active:(T.get_column_Json stmt 1)
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("SELECT\n\
  id,\n\
  JSON_SET(data, '$.active', TRUE) AS data_with_active\n\
FROM people_data_json_kind_never_null_and_col_strict") T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

  end (* module Fold *)
  
  module List = struct
    let two db  callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name:(T.get_column_Json_nullable stmt 1)
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("SELECT id, JSON_EXTRACT(data, '$.name') AS name\n\
FROM people\n\
WHERE JSON_UNQUOTE(JSON_EXTRACT(data, '$.address.city')) = 'Paris'") T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let three db  callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~data_with_active:(T.get_column_Json_nullable stmt 1)
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("SELECT\n\
  id,\n\
  JSON_SET(data, '$.active', TRUE) AS data_with_active\n\
FROM people") T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let four db  callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~data_with_active:(T.get_column_Json_nullable stmt 1)
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("SELECT\n\
  id,\n\
  JSON_SET(data, '$.active', TRUE) AS data_with_active\n\
FROM people_data_json_kind_never_null_but_col_nullable") T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let five db  callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~data_with_active:(T.get_column_Json stmt 1)
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("SELECT\n\
  id,\n\
  JSON_SET(data, '$.active', TRUE) AS data_with_active\n\
FROM people_data_json_kind_never_null_and_col_strict") T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

  end (* module List *)
end (* module Sqlgg *)
