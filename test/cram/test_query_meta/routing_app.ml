open Printf

let current_request = ref "req-1"

module Cache = Sqlgg_stmt_cache.Make (struct
  let max_cache_size = 2
  let ttl_seconds = None
end) (Print_impl)

let static_attrs q = [ "app", "users api"; "query", q.Sqlgg_traits.Query.name ]

let annotate_static q = Sqlgg_traits.Query.Sqlcommenter.annotate (static_attrs q) q

let annotate_per_request q =
  Sqlgg_traits.Query.Sqlcommenter.annotate
    (static_attrs q @ [ "request_id", !current_request ]) q

let per_request q = q.Sqlgg_traits.Query.name = "find_user"

module Routed_db = struct

  include Cache

  let select db q set_params cb =
    if per_request q then Print_impl.select db.Cache.original (annotate_per_request q) set_params cb
    else Cache.select db (annotate_static q) set_params cb

  let select_one db q set_params conv =
    if per_request q then Print_impl.select_one db.Cache.original (annotate_per_request q) set_params conv
    else Cache.select_one db (annotate_static q) set_params conv

  let select_one_maybe db q set_params conv =
    if per_request q then Print_impl.select_one_maybe db.Cache.original (annotate_per_request q) set_params conv
    else Cache.select_one_maybe db (annotate_static q) set_params conv

  let execute db q set_params =
    if per_request q then Print_impl.execute db.Cache.original (annotate_per_request q) set_params
    else Cache.execute db (annotate_static q) set_params

  let execute_unprepared db q =
    if per_request q then Print_impl.execute_unprepared db.Cache.original (annotate_per_request q)
    else Cache.execute_unprepared db (annotate_static q)

end

module Naive_db = struct

  include Cache

  let select db q set_params cb = Cache.select db (annotate_per_request q) set_params cb
  let select_one db q set_params conv = Cache.select_one db (annotate_per_request q) set_params conv
  let select_one_maybe db q set_params conv = Cache.select_one_maybe db (annotate_per_request q) set_params conv
  let execute db q set_params = Cache.execute db (annotate_per_request q) set_params
  let execute_unprepared db q = Cache.execute_unprepared db (annotate_per_request q)

end

module Routed = Output.Sqlgg (Routed_db)
module Naive = Output.Sqlgg (Naive_db)

let section name = printf "\n=== %s ===\n" name

let one_count () =
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_one_response (Some (Print_impl.make_mock_row [ Print_impl.mock_int 7L ]))

let one_user () =
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response
    [ Print_impl.make_mock_row [ Print_impl.mock_int 1L; Print_impl.mock_text "alice" ] ]

let one_update () =
  Print_impl.clear_mock_responses ();
  Print_impl.setup_execute_response ~affected_rows:1L ()

let () =
  let db = Cache.create_cached_connection () in
  Print_impl.reset_mock_stats ();

  section "static attributes fill the cache";
  one_count (); ignore (Routed.select_2 db);
  one_update (); ignore (Routed.rename_user ~name:"carol" ~id:1L db);
  printf "%s\n" (Cache.cache_stats db);

  section "both are reused, comments unchanged";
  one_count (); ignore (Routed.select_2 db);
  one_update (); ignore (Routed.rename_user ~name:"dave" ~id:1L db);
  printf "%s\n" (Cache.cache_stats db);

  section "per request query is routed past the cache, comment is fresh";
  one_user (); Routed.find_user ~id:1L db (fun ~id ~name -> printf "row: %Ld %s\n" id name);
  current_request := "req-2";
  one_user (); Routed.find_user ~id:2L db (fun ~id ~name -> printf "row: %Ld %s\n" id name);
  printf "%s\n" (Cache.cache_stats db);

  section "the cached statements survived, still no new prepare for them";
  one_count (); ignore (Routed.select_2 db);
  one_update (); ignore (Routed.rename_user ~name:"erin" ~id:1L db);
  printf "%s\n" (Cache.cache_stats db);
  printf "--- after routing ---\n";
  ignore (Print_impl.get_mock_stats ());

  section "same load through the cache instead: every call is a new statement";
  let db = Cache.create_cached_connection () in
  Print_impl.reset_mock_stats ();
  one_count (); ignore (Naive.select_2 db);
  one_update (); ignore (Naive.rename_user ~name:"carol" ~id:1L db);
  printf "%s\n" (Cache.cache_stats db);
  current_request := "req-3";
  one_user (); Naive.find_user ~id:1L db (fun ~id ~name -> printf "row: %Ld %s\n" id name);
  current_request := "req-4";
  one_user (); Naive.find_user ~id:2L db (fun ~id ~name -> printf "row: %Ld %s\n" id name);
  printf "%s\n" (Cache.cache_stats db);

  section "and the statements it evicted have to be prepared again";
  one_count (); ignore (Naive.select_2 db);
  one_update (); ignore (Naive.rename_user ~name:"dave" ~id:1L db);
  printf "%s\n" (Cache.cache_stats db);
  printf "--- after naive ---\n";
  ignore (Print_impl.get_mock_stats ())
