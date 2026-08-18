open Printf

let current_request = ref "req-1"
let per_request_attrs = ref []

module Cache = Sqlgg_stmt_cache.Make (struct
  let max_cache_size = 16
  let ttl_seconds = None
end) (Print_impl)

module Annotating_cache = struct

  include Cache

  let annotate q =
    Sqlgg_traits.Query.Sqlcommenter.annotate
      ([ "app", "users api"; "query", q.Sqlgg_traits.Query.name ] @ !per_request_attrs) q

  let select db q = Cache.select db (annotate q)
  let select_one db q = Cache.select_one db (annotate q)
  let select_one_maybe db q = Cache.select_one_maybe db (annotate q)
  let execute db q = Cache.execute db (annotate q)

end

module Sql = Output.Sqlgg (Annotating_cache)

let section name = printf "\n=== %s ===\n" name

let find_user id db =
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response [
    Print_impl.make_mock_row [ Print_impl.mock_int id; Print_impl.mock_text "alice" ]
  ];
  Sql.find_user ~id db (fun ~id ~name -> printf "row: %Ld %s\n" id name)

let () =
  let db = Annotating_cache.create_cached_connection () in
  Print_impl.reset_mock_stats ();

  section "static attributes only: first call prepares";
  find_user 1L db;
  printf "%s\n" (Cache.cache_stats db);

  section "next request reuses that statement, the sql is the same";
  current_request := "req-2";
  find_user 2L db;
  printf "%s\n" (Cache.cache_stats db);

  section "execute takes the same path";
  Print_impl.clear_mock_responses ();
  Print_impl.setup_execute_response ~affected_rows:1L ();
  ignore (Sql.rename_user ~name:"carol" ~id:1L db);
  printf "%s\n" (Cache.cache_stats db);

  section "adding a per request attribute changes the sql on every call";
  per_request_attrs := [ "request_id", !current_request ];
  find_user 3L db;
  printf "%s\n" (Cache.cache_stats db);
  current_request := "req-3";
  per_request_attrs := [ "request_id", !current_request ];
  find_user 4L db;
  printf "%s\n" (Cache.cache_stats db);

  ignore (Print_impl.get_mock_stats ())
