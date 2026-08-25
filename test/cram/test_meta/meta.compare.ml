module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking

  let create_accounts db  =
    T.execute db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE accounts (\n\
  id BIGINT NOT NULL,\n\
    cid BIGINT NOT NULL,\n\
    status TEXT NOT NULL,\n\
  plain TEXT NOT NULL\n\
)") ~name:"create_accounts" ~kind:Sqlgg_traits.Query.(Create "accounts") ()) T.no_params

  let create_orders db  =
    T.execute db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE orders (\n\
    id BIGINT NOT NULL,\n\
    amount DECIMAL(10,2) NOT NULL\n\
)") ~name:"create_orders" ~kind:Sqlgg_traits.Query.(Create "orders") ()) T.no_params

  let create_events db  =
    T.execute db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE events (\n\
  order_ref BIGINT NOT NULL,\n\
  amount BIGINT NOT NULL\n\
)") ~name:"create_events" ~kind:Sqlgg_traits.Query.(Create "events") ()) T.no_params

  let create_named_owners db  =
    T.execute db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE named_owners (\n\
    id BIGINT NOT NULL,\n\
  title TEXT NOT NULL\n\
)") ~name:"create_named_owners" ~kind:Sqlgg_traits.Query.(Create "named_owners") ()) T.no_params

  let create_named_owned db  =
    T.execute db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE named_owned (\n\
  id BIGINT NOT NULL,\n\
  note TEXT NOT NULL\n\
)") ~name:"create_named_owned" ~kind:Sqlgg_traits.Query.(Create "named_owned") ()) T.no_params

  let create_other_domain db  =
    T.execute db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE other_domain (\n\
    id BIGINT NOT NULL\n\
)") ~name:"create_other_domain" ~kind:Sqlgg_traits.Query.(Create "other_domain") ()) T.no_params

  let create_owners db  =
    T.execute db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE owners (\n\
    id BIGINT NOT NULL PRIMARY KEY\n\
)") ~name:"create_owners" ~kind:Sqlgg_traits.Query.(Create "owners") ()) T.no_params

  let create_owned db  =
    T.execute db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE owned (\n\
  owner_ref BIGINT NOT NULL,\n\
  loose BIGINT NOT NULL,\n\
  FOREIGN KEY (owner_ref) REFERENCES owners(id)\n\
)") ~name:"create_owned" ~kind:Sqlgg_traits.Query.(Create "owned") ()) T.no_params

  let create_unrelated db  =
    T.execute db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE unrelated (\n\
  owner_ref BIGINT NOT NULL\n\
)") ~name:"create_unrelated" ~kind:Sqlgg_traits.Query.(Create "unrelated") ()) T.no_params

  let create_projects db  =
    T.execute db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE projects (\n\
  id BIGINT NOT NULL,\n\
    company_id BIGINT NOT NULL\n\
)") ~name:"create_projects" ~kind:Sqlgg_traits.Query.(Create "projects") ()) T.no_params

  let create_alerts db  =
    T.execute db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE alerts (\n\
  dashboard_id BIGINT NOT NULL,\n\
  company_id BIGINT NOT NULL\n\
)") ~name:"create_alerts" ~kind:Sqlgg_traits.Query.(Create "alerts") ()) T.no_params

  let create_courses db  =
    T.execute db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE courses (\n\
    slug TEXT NOT NULL,\n\
    seconds BIGINT NOT NULL\n\
)") ~name:"create_courses" ~kind:Sqlgg_traits.Query.(Create "courses") ()) T.no_params

  let create_left_rows db  =
    T.execute db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE left_rows (\n\
    id BIGINT NOT NULL\n\
)") ~name:"create_left_rows" ~kind:Sqlgg_traits.Query.(Create "left_rows") ()) T.no_params

  let create_right_rows db  =
    T.execute db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE right_rows (\n\
    id BIGINT NOT NULL,\n\
    status ENUM('draft','published','failed') NOT NULL\n\
)") ~name:"create_right_rows" ~kind:Sqlgg_traits.Query.(Create "right_rows") ()) T.no_params

  let create_codec_rows db  =
    T.execute db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE codec_rows (\n\
  id INT NOT NULL,\n\
      status ENUM('new','paid','shipped') NOT NULL\n\
)") ~name:"create_codec_rows" ~kind:Sqlgg_traits.Query.(Create "codec_rows") ()) T.no_params

  let shared_by_equality db  callback =
    let invoke_callback stmt =
      callback
        ~order_ref:(Codecs.Order_id.get_column (T.get_column_int64 stmt 0))
        ~id:(Codecs.Order_id.get_column (T.get_column_int64 stmt 1))
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT events.order_ref, orders.id FROM orders JOIN events ON orders.id = events.order_ref") ~name:"shared_by_equality" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let disjunction_withdraws_it db  callback =
    let invoke_callback stmt =
      callback
        ~order_ref:(T.get_column_Int stmt 0)
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT events.order_ref FROM orders JOIN events ON orders.id = events.order_ref OR events.order_ref = 0") ~name:"disjunction_withdraws_it" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let representation_mismatch db  callback =
    let invoke_callback stmt =
      callback
        ~amount:(T.get_column_Int stmt 0)
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT events.amount FROM orders JOIN events ON orders.amount = events.amount") ~name:"representation_mismatch" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let towards_the_nullable_side db  callback =
    let invoke_callback stmt =
      callback
        ~order_ref:(Codecs.Order_id.get_column_nullable (T.get_column_int64_nullable stmt 0))
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT events.order_ref FROM orders LEFT JOIN events ON orders.id = events.order_ref") ~name:"towards_the_nullable_side" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let towards_the_preserved_side db  callback =
    let invoke_callback stmt =
      callback
        ~order_ref:(T.get_column_Int stmt 0)
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT events.order_ref FROM events LEFT JOIN orders ON orders.id = events.order_ref") ~name:"towards_the_preserved_side" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let no_join db  callback =
    let invoke_callback stmt =
      callback
        ~owner_ref:(Codecs.Owner_id.get_column (T.get_column_int64 stmt 0))
        ~loose:(T.get_column_Int stmt 1)
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT owner_ref, loose FROM owned") ~name:"no_join" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let outer_join_preserved_side db  callback =
    let invoke_callback stmt =
      callback
        ~owner_ref:(Codecs.Owner_id.get_column (T.get_column_int64 stmt 0))
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT owned.owner_ref FROM owned LEFT JOIN owners ON owners.id = owned.owner_ref") ~name:"outer_join_preserved_side" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let without_the_declaration db  callback =
    let invoke_callback stmt =
      callback
        ~owner_ref:(T.get_column_Int stmt 0)
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT unrelated.owner_ref FROM unrelated LEFT JOIN owners ON owners.id = unrelated.owner_ref") ~name:"without_the_declaration" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let through_a_null_handling_call db ~a ~b callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (2) in
      T.set_param_string p (Codecs.Status.set_param a);
      T.set_param_string p (Codecs.Status.set_param b);
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM accounts WHERE IFNULL(?, status) = ?") ~name:"through_a_null_handling_call" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let through_nested_coalesce db ~a ~b callback =
    let invoke_callback stmt =
      callback
        ~c:(Codecs.Cid.get_column (T.get_column_int64 stmt 0))
    in
    let set_params stmt =
      let p = T.start_params stmt (2) in
      T.set_param_int64 p (Codecs.Cid.set_param a);
      T.set_param_int64 p (Codecs.Cid.set_param b);
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT COALESCE(COALESCE(cid, ?), ?) AS c FROM accounts") ~name:"through_nested_coalesce" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let through_nullif db ~param callback =
    let invoke_callback stmt =
      callback
        ~s:(Codecs.Status.get_column_nullable (T.get_column_string_nullable stmt 0))
    in
    let set_params stmt =
      let p = T.start_params stmt (1) in
      T.set_param_string p (Codecs.Status.set_param param);
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT NULLIF(status, ?) AS s FROM accounts") ~name:"through_nullif" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let greatest_over_a_literal db  callback =
    let invoke_callback stmt =
      callback
        ~c:(Codecs.Cid.get_column (T.get_column_int64 stmt 0))
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT GREATEST(cid, 0) AS c FROM accounts") ~name:"greatest_over_a_literal" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let least_feeds_the_param db ~param callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (1) in
      T.set_param_int64 p (Codecs.Cid.set_param param);
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM accounts WHERE cid = LEAST(?, 0)") ~name:"least_feeds_the_param" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let untyped_sibling db  callback =
    let invoke_callback stmt =
      callback
        ~company_id:(Codecs.Company_id.get_column (T.get_column_int64 stmt 0))
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT COALESCE(projects.company_id, alerts.company_id) AS company_id\n\
FROM alerts LEFT JOIN projects ON alerts.dashboard_id = projects.id") ~name:"untyped_sibling" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let literal_fallback db  callback =
    let invoke_callback stmt =
      callback
        ~module_slug:(Codecs.Slug.get_column (T.get_column_string stmt 0))
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT COALESCE(slug, '') AS module_slug FROM courses") ~name:"literal_fallback" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let selecting_aggregate_keeps_it db  =
    let get_row stmt =
      (Codecs.Db_int.get_column (T.get_column_int64 stmt 0))
    in
    T.select_one db (Sqlgg_traits.Query.make ~sql:("SELECT COALESCE(MAX(seconds), 0) AS longest FROM courses") ~name:"selecting_aggregate_keeps_it" ~kind:Sqlgg_traits.Query.(Select One) ()) T.no_params get_row

  let computing_aggregate_keeps_it db  =
    let get_row stmt =
      (Codecs.Db_int.get_column (T.get_column_int64 stmt 0))
    in
    T.select_one db (Sqlgg_traits.Query.make ~sql:("SELECT COALESCE(SUM(seconds), 0) AS total FROM courses") ~name:"computing_aggregate_keeps_it" ~kind:Sqlgg_traits.Query.(Select One) ()) T.no_params get_row

  let transforming_function db ~param callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (1) in
      T.set_param_Text p param;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM accounts WHERE LOWER(status) = ?") ~name:"transforming_function" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let arithmetic db ~param callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (1) in
      T.set_param_Int p param;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM accounts WHERE cid = ? + 1") ~name:"arithmetic" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let concatenation db  callback =
    let invoke_callback stmt =
      callback
        ~s:(T.get_column_Text stmt 0)
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT CONCAT(status, plain) AS s FROM accounts") ~name:"concatenation" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let case_literal_fallback db  callback =
    let invoke_callback stmt =
      callback
        ~s:(Codecs.Status.get_column (T.get_column_string stmt 0))
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT CASE WHEN id = 1 THEN status ELSE 'active' END AS s FROM accounts") ~name:"case_literal_fallback" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let param_branch db ~param callback =
    let invoke_callback stmt =
      callback
        ~s:(Codecs.Status.get_column (T.get_column_string stmt 0))
    in
    let set_params stmt =
      let p = T.start_params stmt (1) in
      T.set_param_string p (Codecs.Status.set_param param);
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT CASE WHEN id = 1 THEN status ELSE ? END AS s FROM accounts") ~name:"param_branch" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let condition_is_not_a_branch db ~cond callback =
    let invoke_callback stmt =
      callback
        ~s:(T.get_column_Text stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (1) in
      T.set_param_string p (Codecs.Status.set_param cond);
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT CASE WHEN status = ? THEN plain ELSE '' END AS s FROM accounts") ~name:"condition_is_not_a_branch" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let fetch_merged db  callback =
    let invoke_callback stmt =
      callback
        ~left_id:(Codecs.Left_id.get_column_nullable (T.get_column_int64_nullable stmt 0))
        ~right_id:(Codecs.Right_id.get_column_nullable (T.get_column_int64_nullable stmt 1))
        ~row_status:(Codecs.Row_status.get_column (T.get_column_string stmt 2))
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT l.id AS left_id, NULL AS right_id, 'published' AS row_status FROM left_rows l\n\
UNION ALL\n\
SELECT NULL AS left_id, r.id AS right_id, r.status AS row_status FROM right_rows r") ~name:"fetch_merged" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let scalar_subquery db  callback =
    let invoke_callback stmt =
      callback
        ~c:(Codecs.Cid.get_column_nullable (T.get_column_int64_nullable stmt 0))
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT (SELECT cid FROM accounts LIMIT 1) AS c FROM accounts") ~name:"scalar_subquery" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let scalar_subquery_with_cte db  callback =
    let invoke_callback stmt =
      callback
        ~c:(Codecs.Cid.get_column_nullable (T.get_column_int64_nullable stmt 0))
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT (WITH c AS (SELECT cid FROM accounts) SELECT cid FROM c LIMIT 1) AS c FROM accounts") ~name:"scalar_subquery_with_cte" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let one_typed_one_not db ~cid ~id callback =
    let invoke_callback stmt =
      callback
        ~plain:(T.get_column_Text stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (2) in
      T.set_param_int64 p (Codecs.Cid.set_param cid);
      T.set_param_Int p id;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT plain FROM accounts WHERE cid = ? AND id = ?") ~name:"one_typed_one_not" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let param_list db ~cids callback =
    let invoke_callback stmt =
      callback
        ~plain:(T.get_column_Text stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (0 + (match cids with [] -> 0 | _ :: _ -> 0)) in
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT plain FROM accounts WHERE " ^ (match cids with [] -> "FALSE" | _ :: _ -> "cid IN " ^  "(" ^ String.concat ", " (List.map (fun v -> T.Types.Int.int64_to_literal (Codecs.Cid.set_param v)) cids) ^ ")")) ~name:"param_list" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let assignment db ~cid ~id =
    let set_params stmt =
      let p = T.start_params stmt (2) in
      T.set_param_int64 p (Codecs.Cid.set_param cid);
      T.set_param_Int p id;
      T.finish_params p
    in
    T.execute db (Sqlgg_traits.Query.make ~sql:("UPDATE accounts SET cid = ? WHERE id = ?") ~name:"assignment" ~kind:Sqlgg_traits.Query.(Update (Some "accounts")) ()) set_params

  let insert_from_select db ~cid =
    let set_params stmt =
      let p = T.start_params stmt (1) in
      T.set_param_int64 p (Codecs.Cid.set_param cid);
      T.finish_params p
    in
    T.execute db (Sqlgg_traits.Query.make ~sql:("INSERT INTO accounts (id, cid, status, plain) SELECT id, ?, status, plain FROM accounts") ~name:"insert_from_select" ~kind:Sqlgg_traits.Query.(Insert "accounts") ()) set_params

  let get_status db ~id callback =
    let invoke_callback stmt =
      callback
        ~status:(Codecs.Order_status.of_db (T.get_column_string stmt 0))
    in
    let set_params stmt =
      let p = T.start_params stmt (1) in
      T.set_param_Int p id;
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT status FROM codec_rows WHERE id = ?") ~name:"get_status" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  let set_status db ~status ~id =
    let set_params stmt =
      let p = T.start_params stmt (2) in
      T.set_param_string p (Codecs.Order_status.to_db status);
      T.set_param_Int p id;
      T.finish_params p
    in
    T.execute db (Sqlgg_traits.Query.make ~sql:("UPDATE codec_rows SET status = ? WHERE id = ?") ~name:"set_status" ~kind:Sqlgg_traits.Query.(Update (Some "codec_rows")) ()) set_params

  let spelled_with_on db  callback =
    let invoke_callback stmt =
      callback
        ~id:(Codecs.Owner_id.get_column (T.get_column_int64 stmt 0))
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT named_owned.id FROM named_owned JOIN named_owners ON named_owned.id = named_owners.id") ~name:"spelled_with_on" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let spelled_with_using db  callback =
    let invoke_callback stmt =
      callback
        ~id:(Codecs.Owner_id.get_column (T.get_column_int64 stmt 0))
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT named_owned.id FROM named_owned JOIN named_owners USING (id)") ~name:"spelled_with_using" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let spelled_naturally db  callback =
    let invoke_callback stmt =
      callback
        ~id:(Codecs.Owner_id.get_column (T.get_column_int64 stmt 0))
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT named_owned.id FROM named_owned NATURAL JOIN named_owners") ~name:"spelled_naturally" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let outer_join_by_name_stays_silent db  callback =
    let invoke_callback stmt =
      callback
        ~id:(T.get_column_Int stmt 0)
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT named_owned.id FROM named_owned LEFT JOIN named_owners USING (id)") ~name:"outer_join_by_name_stays_silent" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params invoke_callback

  let param_meets_two_domains db ~needle callback =
    let invoke_callback stmt =
      callback
        ~title:(T.get_column_Text stmt 0)
    in
    let set_params stmt =
      let p = T.start_params stmt (2) in
      T.set_param_int64 p (Codecs.Owner_id.set_param needle);
      T.set_param_int64 p (Codecs.Course_id.set_param needle);
      T.finish_params p
    in
    T.select db (Sqlgg_traits.Query.make ~sql:("SELECT named_owners.title FROM named_owners, other_domain\n\
WHERE named_owners.id = ? AND other_domain.id = ?") ~name:"param_meets_two_domains" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback

  module Single = struct
    let selecting_aggregate_keeps_it db  callback =
      let invoke_callback stmt =
        callback
          ~longest:(Codecs.Db_int.get_column (T.get_column_int64 stmt 0))
      in
      T.select_one db (Sqlgg_traits.Query.make ~sql:("SELECT COALESCE(MAX(seconds), 0) AS longest FROM courses") ~name:"selecting_aggregate_keeps_it" ~kind:Sqlgg_traits.Query.(Select One) ()) T.no_params invoke_callback

    let computing_aggregate_keeps_it db  callback =
      let invoke_callback stmt =
        callback
          ~total:(Codecs.Db_int.get_column (T.get_column_int64 stmt 0))
      in
      T.select_one db (Sqlgg_traits.Query.make ~sql:("SELECT COALESCE(SUM(seconds), 0) AS total FROM courses") ~name:"computing_aggregate_keeps_it" ~kind:Sqlgg_traits.Query.(Select One) ()) T.no_params invoke_callback

  end (* module Single *)
  
  module Fold = struct
    let shared_by_equality db  callback acc =
      let invoke_callback stmt =
        callback
          ~order_ref:(Codecs.Order_id.get_column (T.get_column_int64 stmt 0))
          ~id:(Codecs.Order_id.get_column (T.get_column_int64 stmt 1))
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT events.order_ref, orders.id FROM orders JOIN events ON orders.id = events.order_ref") ~name:"shared_by_equality" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let disjunction_withdraws_it db  callback acc =
      let invoke_callback stmt =
        callback
          ~order_ref:(T.get_column_Int stmt 0)
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT events.order_ref FROM orders JOIN events ON orders.id = events.order_ref OR events.order_ref = 0") ~name:"disjunction_withdraws_it" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let representation_mismatch db  callback acc =
      let invoke_callback stmt =
        callback
          ~amount:(T.get_column_Int stmt 0)
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT events.amount FROM orders JOIN events ON orders.amount = events.amount") ~name:"representation_mismatch" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let towards_the_nullable_side db  callback acc =
      let invoke_callback stmt =
        callback
          ~order_ref:(Codecs.Order_id.get_column_nullable (T.get_column_int64_nullable stmt 0))
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT events.order_ref FROM orders LEFT JOIN events ON orders.id = events.order_ref") ~name:"towards_the_nullable_side" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let towards_the_preserved_side db  callback acc =
      let invoke_callback stmt =
        callback
          ~order_ref:(T.get_column_Int stmt 0)
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT events.order_ref FROM events LEFT JOIN orders ON orders.id = events.order_ref") ~name:"towards_the_preserved_side" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let no_join db  callback acc =
      let invoke_callback stmt =
        callback
          ~owner_ref:(Codecs.Owner_id.get_column (T.get_column_int64 stmt 0))
          ~loose:(T.get_column_Int stmt 1)
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT owner_ref, loose FROM owned") ~name:"no_join" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let outer_join_preserved_side db  callback acc =
      let invoke_callback stmt =
        callback
          ~owner_ref:(Codecs.Owner_id.get_column (T.get_column_int64 stmt 0))
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT owned.owner_ref FROM owned LEFT JOIN owners ON owners.id = owned.owner_ref") ~name:"outer_join_preserved_side" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let without_the_declaration db  callback acc =
      let invoke_callback stmt =
        callback
          ~owner_ref:(T.get_column_Int stmt 0)
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT unrelated.owner_ref FROM unrelated LEFT JOIN owners ON owners.id = unrelated.owner_ref") ~name:"without_the_declaration" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let through_a_null_handling_call db ~a ~b callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_string p (Codecs.Status.set_param a);
        T.set_param_string p (Codecs.Status.set_param b);
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM accounts WHERE IFNULL(?, status) = ?") ~name:"through_a_null_handling_call" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let through_nested_coalesce db ~a ~b callback acc =
      let invoke_callback stmt =
        callback
          ~c:(Codecs.Cid.get_column (T.get_column_int64 stmt 0))
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_int64 p (Codecs.Cid.set_param a);
        T.set_param_int64 p (Codecs.Cid.set_param b);
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT COALESCE(COALESCE(cid, ?), ?) AS c FROM accounts") ~name:"through_nested_coalesce" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let through_nullif db ~param callback acc =
      let invoke_callback stmt =
        callback
          ~s:(Codecs.Status.get_column_nullable (T.get_column_string_nullable stmt 0))
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_string p (Codecs.Status.set_param param);
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT NULLIF(status, ?) AS s FROM accounts") ~name:"through_nullif" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let greatest_over_a_literal db  callback acc =
      let invoke_callback stmt =
        callback
          ~c:(Codecs.Cid.get_column (T.get_column_int64 stmt 0))
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT GREATEST(cid, 0) AS c FROM accounts") ~name:"greatest_over_a_literal" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let least_feeds_the_param db ~param callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_int64 p (Codecs.Cid.set_param param);
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM accounts WHERE cid = LEAST(?, 0)") ~name:"least_feeds_the_param" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let untyped_sibling db  callback acc =
      let invoke_callback stmt =
        callback
          ~company_id:(Codecs.Company_id.get_column (T.get_column_int64 stmt 0))
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT COALESCE(projects.company_id, alerts.company_id) AS company_id\n\
FROM alerts LEFT JOIN projects ON alerts.dashboard_id = projects.id") ~name:"untyped_sibling" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let literal_fallback db  callback acc =
      let invoke_callback stmt =
        callback
          ~module_slug:(Codecs.Slug.get_column (T.get_column_string stmt 0))
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT COALESCE(slug, '') AS module_slug FROM courses") ~name:"literal_fallback" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let transforming_function db ~param callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Text p param;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM accounts WHERE LOWER(status) = ?") ~name:"transforming_function" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let arithmetic db ~param callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p param;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM accounts WHERE cid = ? + 1") ~name:"arithmetic" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let concatenation db  callback acc =
      let invoke_callback stmt =
        callback
          ~s:(T.get_column_Text stmt 0)
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT CONCAT(status, plain) AS s FROM accounts") ~name:"concatenation" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let case_literal_fallback db  callback acc =
      let invoke_callback stmt =
        callback
          ~s:(Codecs.Status.get_column (T.get_column_string stmt 0))
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT CASE WHEN id = 1 THEN status ELSE 'active' END AS s FROM accounts") ~name:"case_literal_fallback" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let param_branch db ~param callback acc =
      let invoke_callback stmt =
        callback
          ~s:(Codecs.Status.get_column (T.get_column_string stmt 0))
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_string p (Codecs.Status.set_param param);
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT CASE WHEN id = 1 THEN status ELSE ? END AS s FROM accounts") ~name:"param_branch" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let condition_is_not_a_branch db ~cond callback acc =
      let invoke_callback stmt =
        callback
          ~s:(T.get_column_Text stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_string p (Codecs.Status.set_param cond);
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT CASE WHEN status = ? THEN plain ELSE '' END AS s FROM accounts") ~name:"condition_is_not_a_branch" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let fetch_merged db  callback acc =
      let invoke_callback stmt =
        callback
          ~left_id:(Codecs.Left_id.get_column_nullable (T.get_column_int64_nullable stmt 0))
          ~right_id:(Codecs.Right_id.get_column_nullable (T.get_column_int64_nullable stmt 1))
          ~row_status:(Codecs.Row_status.get_column (T.get_column_string stmt 2))
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT l.id AS left_id, NULL AS right_id, 'published' AS row_status FROM left_rows l\n\
UNION ALL\n\
SELECT NULL AS left_id, r.id AS right_id, r.status AS row_status FROM right_rows r") ~name:"fetch_merged" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let scalar_subquery db  callback acc =
      let invoke_callback stmt =
        callback
          ~c:(Codecs.Cid.get_column_nullable (T.get_column_int64_nullable stmt 0))
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT (SELECT cid FROM accounts LIMIT 1) AS c FROM accounts") ~name:"scalar_subquery" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let scalar_subquery_with_cte db  callback acc =
      let invoke_callback stmt =
        callback
          ~c:(Codecs.Cid.get_column_nullable (T.get_column_int64_nullable stmt 0))
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT (WITH c AS (SELECT cid FROM accounts) SELECT cid FROM c LIMIT 1) AS c FROM accounts") ~name:"scalar_subquery_with_cte" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let one_typed_one_not db ~cid ~id callback acc =
      let invoke_callback stmt =
        callback
          ~plain:(T.get_column_Text stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_int64 p (Codecs.Cid.set_param cid);
        T.set_param_Int p id;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT plain FROM accounts WHERE cid = ? AND id = ?") ~name:"one_typed_one_not" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let param_list db ~cids callback acc =
      let invoke_callback stmt =
        callback
          ~plain:(T.get_column_Text stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match cids with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT plain FROM accounts WHERE " ^ (match cids with [] -> "FALSE" | _ :: _ -> "cid IN " ^  "(" ^ String.concat ", " (List.map (fun v -> T.Types.Int.int64_to_literal (Codecs.Cid.set_param v)) cids) ^ ")")) ~name:"param_list" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let get_status db ~id callback acc =
      let invoke_callback stmt =
        callback
          ~status:(Codecs.Order_status.of_db (T.get_column_string stmt 0))
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p id;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT status FROM codec_rows WHERE id = ?") ~name:"get_status" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let spelled_with_on db  callback acc =
      let invoke_callback stmt =
        callback
          ~id:(Codecs.Owner_id.get_column (T.get_column_int64 stmt 0))
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT named_owned.id FROM named_owned JOIN named_owners ON named_owned.id = named_owners.id") ~name:"spelled_with_on" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let spelled_with_using db  callback acc =
      let invoke_callback stmt =
        callback
          ~id:(Codecs.Owner_id.get_column (T.get_column_int64 stmt 0))
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT named_owned.id FROM named_owned JOIN named_owners USING (id)") ~name:"spelled_with_using" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let spelled_naturally db  callback acc =
      let invoke_callback stmt =
        callback
          ~id:(Codecs.Owner_id.get_column (T.get_column_int64 stmt 0))
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT named_owned.id FROM named_owned NATURAL JOIN named_owners") ~name:"spelled_naturally" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let outer_join_by_name_stays_silent db  callback acc =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT named_owned.id FROM named_owned LEFT JOIN named_owners USING (id)") ~name:"outer_join_by_name_stays_silent" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

    let param_meets_two_domains db ~needle callback acc =
      let invoke_callback stmt =
        callback
          ~title:(T.get_column_Text stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_int64 p (Codecs.Owner_id.set_param needle);
        T.set_param_int64 p (Codecs.Course_id.set_param needle);
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT named_owners.title FROM named_owners, other_domain\n\
WHERE named_owners.id = ? AND other_domain.id = ?") ~name:"param_meets_two_domains" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

  end (* module Fold *)
  
  module List = struct
    let shared_by_equality db  callback =
      let invoke_callback stmt =
        callback
          ~order_ref:(Codecs.Order_id.get_column (T.get_column_int64 stmt 0))
          ~id:(Codecs.Order_id.get_column (T.get_column_int64 stmt 1))
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT events.order_ref, orders.id FROM orders JOIN events ON orders.id = events.order_ref") ~name:"shared_by_equality" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let disjunction_withdraws_it db  callback =
      let invoke_callback stmt =
        callback
          ~order_ref:(T.get_column_Int stmt 0)
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT events.order_ref FROM orders JOIN events ON orders.id = events.order_ref OR events.order_ref = 0") ~name:"disjunction_withdraws_it" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let representation_mismatch db  callback =
      let invoke_callback stmt =
        callback
          ~amount:(T.get_column_Int stmt 0)
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT events.amount FROM orders JOIN events ON orders.amount = events.amount") ~name:"representation_mismatch" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let towards_the_nullable_side db  callback =
      let invoke_callback stmt =
        callback
          ~order_ref:(Codecs.Order_id.get_column_nullable (T.get_column_int64_nullable stmt 0))
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT events.order_ref FROM orders LEFT JOIN events ON orders.id = events.order_ref") ~name:"towards_the_nullable_side" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let towards_the_preserved_side db  callback =
      let invoke_callback stmt =
        callback
          ~order_ref:(T.get_column_Int stmt 0)
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT events.order_ref FROM events LEFT JOIN orders ON orders.id = events.order_ref") ~name:"towards_the_preserved_side" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let no_join db  callback =
      let invoke_callback stmt =
        callback
          ~owner_ref:(Codecs.Owner_id.get_column (T.get_column_int64 stmt 0))
          ~loose:(T.get_column_Int stmt 1)
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT owner_ref, loose FROM owned") ~name:"no_join" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let outer_join_preserved_side db  callback =
      let invoke_callback stmt =
        callback
          ~owner_ref:(Codecs.Owner_id.get_column (T.get_column_int64 stmt 0))
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT owned.owner_ref FROM owned LEFT JOIN owners ON owners.id = owned.owner_ref") ~name:"outer_join_preserved_side" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let without_the_declaration db  callback =
      let invoke_callback stmt =
        callback
          ~owner_ref:(T.get_column_Int stmt 0)
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT unrelated.owner_ref FROM unrelated LEFT JOIN owners ON owners.id = unrelated.owner_ref") ~name:"without_the_declaration" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let through_a_null_handling_call db ~a ~b callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_string p (Codecs.Status.set_param a);
        T.set_param_string p (Codecs.Status.set_param b);
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM accounts WHERE IFNULL(?, status) = ?") ~name:"through_a_null_handling_call" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let through_nested_coalesce db ~a ~b callback =
      let invoke_callback stmt =
        callback
          ~c:(Codecs.Cid.get_column (T.get_column_int64 stmt 0))
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_int64 p (Codecs.Cid.set_param a);
        T.set_param_int64 p (Codecs.Cid.set_param b);
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT COALESCE(COALESCE(cid, ?), ?) AS c FROM accounts") ~name:"through_nested_coalesce" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let through_nullif db ~param callback =
      let invoke_callback stmt =
        callback
          ~s:(Codecs.Status.get_column_nullable (T.get_column_string_nullable stmt 0))
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_string p (Codecs.Status.set_param param);
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT NULLIF(status, ?) AS s FROM accounts") ~name:"through_nullif" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let greatest_over_a_literal db  callback =
      let invoke_callback stmt =
        callback
          ~c:(Codecs.Cid.get_column (T.get_column_int64 stmt 0))
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT GREATEST(cid, 0) AS c FROM accounts") ~name:"greatest_over_a_literal" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let least_feeds_the_param db ~param callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_int64 p (Codecs.Cid.set_param param);
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM accounts WHERE cid = LEAST(?, 0)") ~name:"least_feeds_the_param" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let untyped_sibling db  callback =
      let invoke_callback stmt =
        callback
          ~company_id:(Codecs.Company_id.get_column (T.get_column_int64 stmt 0))
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT COALESCE(projects.company_id, alerts.company_id) AS company_id\n\
FROM alerts LEFT JOIN projects ON alerts.dashboard_id = projects.id") ~name:"untyped_sibling" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let literal_fallback db  callback =
      let invoke_callback stmt =
        callback
          ~module_slug:(Codecs.Slug.get_column (T.get_column_string stmt 0))
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT COALESCE(slug, '') AS module_slug FROM courses") ~name:"literal_fallback" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let transforming_function db ~param callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Text p param;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM accounts WHERE LOWER(status) = ?") ~name:"transforming_function" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let arithmetic db ~param callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p param;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM accounts WHERE cid = ? + 1") ~name:"arithmetic" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let concatenation db  callback =
      let invoke_callback stmt =
        callback
          ~s:(T.get_column_Text stmt 0)
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT CONCAT(status, plain) AS s FROM accounts") ~name:"concatenation" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let case_literal_fallback db  callback =
      let invoke_callback stmt =
        callback
          ~s:(Codecs.Status.get_column (T.get_column_string stmt 0))
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT CASE WHEN id = 1 THEN status ELSE 'active' END AS s FROM accounts") ~name:"case_literal_fallback" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let param_branch db ~param callback =
      let invoke_callback stmt =
        callback
          ~s:(Codecs.Status.get_column (T.get_column_string stmt 0))
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_string p (Codecs.Status.set_param param);
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT CASE WHEN id = 1 THEN status ELSE ? END AS s FROM accounts") ~name:"param_branch" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let condition_is_not_a_branch db ~cond callback =
      let invoke_callback stmt =
        callback
          ~s:(T.get_column_Text stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_string p (Codecs.Status.set_param cond);
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT CASE WHEN status = ? THEN plain ELSE '' END AS s FROM accounts") ~name:"condition_is_not_a_branch" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let fetch_merged db  callback =
      let invoke_callback stmt =
        callback
          ~left_id:(Codecs.Left_id.get_column_nullable (T.get_column_int64_nullable stmt 0))
          ~right_id:(Codecs.Right_id.get_column_nullable (T.get_column_int64_nullable stmt 1))
          ~row_status:(Codecs.Row_status.get_column (T.get_column_string stmt 2))
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT l.id AS left_id, NULL AS right_id, 'published' AS row_status FROM left_rows l\n\
UNION ALL\n\
SELECT NULL AS left_id, r.id AS right_id, r.status AS row_status FROM right_rows r") ~name:"fetch_merged" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let scalar_subquery db  callback =
      let invoke_callback stmt =
        callback
          ~c:(Codecs.Cid.get_column_nullable (T.get_column_int64_nullable stmt 0))
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT (SELECT cid FROM accounts LIMIT 1) AS c FROM accounts") ~name:"scalar_subquery" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let scalar_subquery_with_cte db  callback =
      let invoke_callback stmt =
        callback
          ~c:(Codecs.Cid.get_column_nullable (T.get_column_int64_nullable stmt 0))
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT (WITH c AS (SELECT cid FROM accounts) SELECT cid FROM c LIMIT 1) AS c FROM accounts") ~name:"scalar_subquery_with_cte" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let one_typed_one_not db ~cid ~id callback =
      let invoke_callback stmt =
        callback
          ~plain:(T.get_column_Text stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_int64 p (Codecs.Cid.set_param cid);
        T.set_param_Int p id;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT plain FROM accounts WHERE cid = ? AND id = ?") ~name:"one_typed_one_not" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let param_list db ~cids callback =
      let invoke_callback stmt =
        callback
          ~plain:(T.get_column_Text stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match cids with [] -> 0 | _ :: _ -> 0)) in
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT plain FROM accounts WHERE " ^ (match cids with [] -> "FALSE" | _ :: _ -> "cid IN " ^  "(" ^ String.concat ", " (List.map (fun v -> T.Types.Int.int64_to_literal (Codecs.Cid.set_param v)) cids) ^ ")")) ~name:"param_list" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let get_status db ~id callback =
      let invoke_callback stmt =
        callback
          ~status:(Codecs.Order_status.of_db (T.get_column_string stmt 0))
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p id;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT status FROM codec_rows WHERE id = ?") ~name:"get_status" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let spelled_with_on db  callback =
      let invoke_callback stmt =
        callback
          ~id:(Codecs.Owner_id.get_column (T.get_column_int64 stmt 0))
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT named_owned.id FROM named_owned JOIN named_owners ON named_owned.id = named_owners.id") ~name:"spelled_with_on" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let spelled_with_using db  callback =
      let invoke_callback stmt =
        callback
          ~id:(Codecs.Owner_id.get_column (T.get_column_int64 stmt 0))
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT named_owned.id FROM named_owned JOIN named_owners USING (id)") ~name:"spelled_with_using" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let spelled_naturally db  callback =
      let invoke_callback stmt =
        callback
          ~id:(Codecs.Owner_id.get_column (T.get_column_int64 stmt 0))
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT named_owned.id FROM named_owned NATURAL JOIN named_owners") ~name:"spelled_naturally" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let outer_join_by_name_stays_silent db  callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT named_owned.id FROM named_owned LEFT JOIN named_owners USING (id)") ~name:"outer_join_by_name_stays_silent" ~kind:Sqlgg_traits.Query.(Select Nat) ()) T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

    let param_meets_two_domains db ~needle callback =
      let invoke_callback stmt =
        callback
          ~title:(T.get_column_Text stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (2) in
        T.set_param_int64 p (Codecs.Owner_id.set_param needle);
        T.set_param_int64 p (Codecs.Course_id.set_param needle);
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT named_owners.title FROM named_owners, other_domain\n\
WHERE named_owners.id = ? AND other_domain.id = ?") ~name:"param_meets_two_domains" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

  end (* module List *)
end (* module Sqlgg *)
