module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking

  let create_categories db  =
    T.execute db ("CREATE TABLE categories (\n\
    id SERIAL PRIMARY KEY,\n\
    name VARCHAR(255) NOT NULL\n\
)") T.no_params

  let create_products db  =
    T.execute db ("CREATE TABLE products (\n\
    id SERIAL PRIMARY KEY,\n\
    name VARCHAR(255) NOT NULL,\n\
    category_id INTEGER,\n\
    price NUMERIC(10, 2) NOT NULL,\n\
    FOREIGN KEY (category_id) REFERENCES categories(id)\n\
)") T.no_params

  let test2 db ~five ~param ~test ~test2 ~test5 callback =
    let invoke_callback stmt =
      callback
        ~y2:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (4 + (match param with `None -> 0 | `Some -> 0)) in
      T.set_param_Int p five;
      T.set_param_Int p test;
      T.set_param_Int p test2;
      T.set_param_Int p test5;
      T.finish_params p
    in
    T.select db ("WITH x AS (SELECT \n\
    1 as y, \n\
    4 + ? as y1\n\
FROM ( \n\
        SELECT 1 AS id, 'l' AS creator_name\n\
        UNION ALL\n\
        SELECT 2, 'k'\n\
) AS x \n\
WHERE " ^ (match param with `None -> " TRUE " | `Some -> " FALSE ") ^ ")\n\
SELECT 1 + ? - ? + ? + x.y1 as y2\n\
FROM x") set_params invoke_callback

  let test3 db ~five ~param ~test ~test2 ~test5 callback =
    let invoke_callback stmt =
      callback
        ~y2:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (4 + (match param with `None -> 0 | `Some -> 0)) in
      T.set_param_Int p five;
      T.set_param_Int p test;
      T.set_param_Int p test2;
      T.set_param_Int p test5;
      T.finish_params p
    in
    T.select db ("WITH x AS (SELECT \n\
    1 as y, \n\
    4 + ? as y1\n\
FROM ( \n\
        SELECT 1 AS id, 'l' AS creator_name\n\
        UNION ALL\n\
        SELECT 2, 'k'\n\
) AS x \n\
WHERE " ^ (match param with `None -> " TRUE " | `Some -> " FALSE ") ^ ")\n\
SELECT 1 + ? - ? + ? + x.y1 as y2\n\
FROM x") set_params invoke_callback

  let reuseme db  callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
        ~name:(T.get_column_Text stmt 1)
        ~category_id:(T.get_column_Int_nullable stmt 2)
        ~category_name:(T.get_column_Text stmt 3)
    in
    T.select db ("WITH inner_cte AS (\n\
        SELECT id, name, category_id \n\
        FROM products \n\
        WHERE price > 100\n\
    )\n\
SELECT inner_cte.*, c.name AS category_name\n\
FROM inner_cte\n\
JOIN categories c ON inner_cte.category_id = c.id") T.no_params invoke_callback

  let reuse_reusable db  callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
        ~name:(T.get_column_Text stmt 1)
        ~category_id:(T.get_column_Int_nullable stmt 2)
        ~category_name:(T.get_column_Text stmt 3)
    in
    let set_params stmt =
      let p = T.start_params stmt (0) in
      T.finish_params p
    in
    T.select db ("WITH outer_cte AS (WITH inner_cte AS (\n\
        SELECT id, name, category_id \n\
        FROM products \n\
        WHERE price > 100\n\
    )\n\
SELECT inner_cte.*, c.name AS category_name\n\
FROM inner_cte\n\
JOIN categories c ON inner_cte.category_id = c.id)\n\
SELECT * FROM outer_cte") set_params invoke_callback

  module Fold = struct
    let test2 db ~five ~param ~test ~test2 ~test5 callback acc =
      let invoke_callback stmt =
        callback
          ~y2:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (4 + (match param with `None -> 0 | `Some -> 0)) in
        T.set_param_Int p five;
        T.set_param_Int p test;
        T.set_param_Int p test2;
        T.set_param_Int p test5;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("WITH x AS (SELECT \n\
    1 as y, \n\
    4 + ? as y1\n\
FROM ( \n\
        SELECT 1 AS id, 'l' AS creator_name\n\
        UNION ALL\n\
        SELECT 2, 'k'\n\
) AS x \n\
WHERE " ^ (match param with `None -> " TRUE " | `Some -> " FALSE ") ^ ")\n\
SELECT 1 + ? - ? + ? + x.y1 as y2\n\
FROM x") set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let test3 db ~five ~param ~test ~test2 ~test5 callback acc =
      let invoke_callback stmt =
        callback
          ~y2:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (4 + (match param with `None -> 0 | `Some -> 0)) in
        T.set_param_Int p five;
        T.set_param_Int p test;
        T.set_param_Int p test2;
        T.set_param_Int p test5;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("WITH x AS (SELECT \n\
    1 as y, \n\
    4 + ? as y1\n\
FROM ( \n\
        SELECT 1 AS id, 'l' AS creator_name\n\
        UNION ALL\n\
        SELECT 2, 'k'\n\
) AS x \n\
WHERE " ^ (match param with `None -> " TRUE " | `Some -> " FALSE ") ^ ")\n\
SELECT 1 + ? - ? + ? + x.y1 as y2\n\
FROM x") set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let reuseme db  callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name:(T.get_column_Text stmt 1)
          ~category_id:(T.get_column_Int_nullable stmt 2)
          ~category_name:(T.get_column_Text stmt 3)
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("WITH inner_cte AS (\n\
        SELECT id, name, category_id \n\
        FROM products \n\
        WHERE price > 100\n\
    )\n\
SELECT inner_cte.*, c.name AS category_name\n\
FROM inner_cte\n\
JOIN categories c ON inner_cte.category_id = c.id") T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let reuse_reusable db  callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name:(T.get_column_Text stmt 1)
          ~category_id:(T.get_column_Int_nullable stmt 2)
          ~category_name:(T.get_column_Text stmt 3)
      in
      let set_params stmt =
        let p = T.start_params stmt (0) in
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("WITH outer_cte AS (WITH inner_cte AS (\n\
        SELECT id, name, category_id \n\
        FROM products \n\
        WHERE price > 100\n\
    )\n\
SELECT inner_cte.*, c.name AS category_name\n\
FROM inner_cte\n\
JOIN categories c ON inner_cte.category_id = c.id)\n\
SELECT * FROM outer_cte") set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

  end (* module Fold *)
  
  module List = struct
    let test2 db ~five ~param ~test ~test2 ~test5 callback =
      let invoke_callback stmt =
        callback
          ~y2:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (4 + (match param with `None -> 0 | `Some -> 0)) in
        T.set_param_Int p five;
        T.set_param_Int p test;
        T.set_param_Int p test2;
        T.set_param_Int p test5;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("WITH x AS (SELECT \n\
    1 as y, \n\
    4 + ? as y1\n\
FROM ( \n\
        SELECT 1 AS id, 'l' AS creator_name\n\
        UNION ALL\n\
        SELECT 2, 'k'\n\
) AS x \n\
WHERE " ^ (match param with `None -> " TRUE " | `Some -> " FALSE ") ^ ")\n\
SELECT 1 + ? - ? + ? + x.y1 as y2\n\
FROM x") set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let test3 db ~five ~param ~test ~test2 ~test5 callback =
      let invoke_callback stmt =
        callback
          ~y2:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (4 + (match param with `None -> 0 | `Some -> 0)) in
        T.set_param_Int p five;
        T.set_param_Int p test;
        T.set_param_Int p test2;
        T.set_param_Int p test5;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("WITH x AS (SELECT \n\
    1 as y, \n\
    4 + ? as y1\n\
FROM ( \n\
        SELECT 1 AS id, 'l' AS creator_name\n\
        UNION ALL\n\
        SELECT 2, 'k'\n\
) AS x \n\
WHERE " ^ (match param with `None -> " TRUE " | `Some -> " FALSE ") ^ ")\n\
SELECT 1 + ? - ? + ? + x.y1 as y2\n\
FROM x") set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let reuseme db  callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name:(T.get_column_Text stmt 1)
          ~category_id:(T.get_column_Int_nullable stmt 2)
          ~category_name:(T.get_column_Text stmt 3)
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("WITH inner_cte AS (\n\
        SELECT id, name, category_id \n\
        FROM products \n\
        WHERE price > 100\n\
    )\n\
SELECT inner_cte.*, c.name AS category_name\n\
FROM inner_cte\n\
JOIN categories c ON inner_cte.category_id = c.id") T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let reuse_reusable db  callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name:(T.get_column_Text stmt 1)
          ~category_id:(T.get_column_Int_nullable stmt 2)
          ~category_name:(T.get_column_Text stmt 3)
      in
      let set_params stmt =
        let p = T.start_params stmt (0) in
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("WITH outer_cte AS (WITH inner_cte AS (\n\
        SELECT id, name, category_id \n\
        FROM products \n\
        WHERE price > 100\n\
    )\n\
SELECT inner_cte.*, c.name AS category_name\n\
FROM inner_cte\n\
JOIN categories c ON inner_cte.category_id = c.id)\n\
SELECT * FROM outer_cte") set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

  end (* module List *)
end (* module Sqlgg *)
