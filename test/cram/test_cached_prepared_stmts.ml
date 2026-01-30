open Printf

module Tiny_Cache_Config = struct
  let max_cache_size = 1
  let ttl_seconds = None
end

module Small_Cache_Config = struct
  let max_cache_size = 2
  let ttl_seconds = None
end

module Medium_Cache_Config = struct
  let max_cache_size = 5
  let ttl_seconds = None
end

module Large_Cache_Config = struct
  let max_cache_size = 10
  let ttl_seconds = None
end

module Impl = (Print_impl : Sqlgg_stmt_cache.Cached_m)

module Tiny_Cache = Sqlgg_stmt_cache.Make(Tiny_Cache_Config)(Print_impl)
module Small_Cache = Sqlgg_stmt_cache.Make(Small_Cache_Config)(Print_impl)
module Medium_Cache = Sqlgg_stmt_cache.Make(Medium_Cache_Config)(Print_impl)
module Large_Cache = Sqlgg_stmt_cache.Make(Large_Cache_Config)(Print_impl)

let setup_mock_responses n =
  for i = 1 to n do
    Print_impl.setup_select_response [Print_impl.make_mock_row [Print_impl.mock_int (Int64.of_int i)]]
  done

let () =
  printf "\n=== TEST: Sequential Filling (TINY, size=1) ===\n";
  let tiny_conn = Tiny_Cache.create_cached_connection () in
  Print_impl.reset_mock_stats ();
  Print_impl.clear_mock_responses ();
  
  setup_mock_responses 1;
  Tiny_Cache.select tiny_conn "SELECT 1" 
    (fun stmt -> Tiny_Cache.finish_params (Tiny_Cache.start_params stmt 0)) 
    (fun _ -> ());
  printf "After query 1: %d prepared, %s\n" !Print_impl.stmt_counter (Tiny_Cache.cache_stats tiny_conn);
  
  let after_fill = !Print_impl.stmt_counter in
  assert (after_fill = 1);
  printf "Filled tiny cache: %d/1\n" after_fill;
  
  Print_impl.setup_select_response [Print_impl.make_mock_row [Print_impl.mock_int 1L]];
  Tiny_Cache.select tiny_conn "SELECT 1" 
    (fun stmt -> Tiny_Cache.finish_params (Tiny_Cache.start_params stmt 0)) 
    (fun _ -> ());
  
  let after_reuse = !Print_impl.stmt_counter in
  printf "Reused cached: %d prepared (no new preparations)\n" after_reuse;
  assert (after_reuse = after_fill);
  printf "Sequential filling test passed for TINY!\n";
  
  printf "\n=== TEST: Sequential Filling (SMALL, size=2) ===\n";
  let small_conn = Small_Cache.create_cached_connection () in
  Print_impl.reset_mock_stats ();
  Print_impl.clear_mock_responses ();
  
  setup_mock_responses 2;
  for i = 1 to 2 do
    let sql = sprintf "SELECT %d" i in
    Small_Cache.select small_conn sql 
      (fun stmt -> Small_Cache.finish_params (Small_Cache.start_params stmt 0)) 
      (fun _ -> ());
    printf "  After query %d: %d prepared, %s\n" i !Print_impl.stmt_counter (Small_Cache.cache_stats small_conn);
  done;
  
  let after_fill = !Print_impl.stmt_counter in
  assert (after_fill = 2);
  printf "Filled small cache: %d/2\n" after_fill;
  printf "Sequential filling test passed for SMALL!\n";
  
  printf "\n=== TEST: Overflow Eviction (SMALL, size=2, overflow=3) ===\n";
  let small_conn2 = Small_Cache.create_cached_connection () in
  Print_impl.reset_mock_stats ();
  Print_impl.clear_mock_responses ();
  
  let total_queries = 5 in
  setup_mock_responses total_queries;
  
  for i = 1 to total_queries do
    let sql = sprintf "SELECT %d" i in
    Small_Cache.select small_conn2 sql 
      (fun stmt -> Small_Cache.finish_params (Small_Cache.start_params stmt 0)) 
      (fun _ -> ());
    
    if i <= 2 then
      printf "  Fill %d/2: %d prepared, %s\n" i !Print_impl.stmt_counter (Small_Cache.cache_stats small_conn2)
    else
      printf "  Overflow %d: %d prepared, %s\n" (i - 2) !Print_impl.stmt_counter (Small_Cache.cache_stats small_conn2);
  done;
  
  let after_overflow = !Print_impl.stmt_counter in
  printf "After overflow: %d prepared (expected %d)\n" after_overflow total_queries;
  assert (after_overflow = total_queries);
  
  setup_mock_responses 3;
  let evicted_preparations = ref 0 in
  for i = 1 to 3 do
    let sql = sprintf "SELECT %d" i in
    let before_prep = !Print_impl.stmt_counter in
    Small_Cache.select small_conn2 sql 
      (fun stmt -> Small_Cache.finish_params (Small_Cache.start_params stmt 0)) 
      (fun _ -> ());
    let after_prep = !Print_impl.stmt_counter in
    if after_prep > before_prep then incr evicted_preparations;
  done;
  
  printf "Evicted queries needed re-preparation: %d/3\n" !evicted_preparations;
  assert (!evicted_preparations = 3);
  printf "Overflow eviction test passed for SMALL!\n";
  
  printf "\n=== TEST: LRU Pattern (MEDIUM, size=5) ===\n";
  let medium_conn = Medium_Cache.create_cached_connection () in
  Print_impl.reset_mock_stats ();
  Print_impl.clear_mock_responses ();
  
  setup_mock_responses 15;
  
  for i = 1 to 5 do
    let sql = sprintf "SELECT %d" i in
    Medium_Cache.select medium_conn sql 
      (fun stmt -> Medium_Cache.finish_params (Medium_Cache.start_params stmt 0)) 
      (fun _ -> ());
  done;
  
  printf "Filled cache: %d prepared\n" !Print_impl.stmt_counter;
  
  Medium_Cache.select medium_conn "SELECT 1" 
    (fun stmt -> Medium_Cache.finish_params (Medium_Cache.start_params stmt 0)) 
    (fun _ -> ());
  
  printf "Used first query (made it recent)\n";
  
  Medium_Cache.select medium_conn "SELECT 6"
    (fun stmt -> Medium_Cache.finish_params (Medium_Cache.start_params stmt 0)) 
    (fun _ -> ());
  
  printf "Added new query: %d prepared, %s\n" !Print_impl.stmt_counter (Medium_Cache.cache_stats medium_conn);
  
  let before_first = !Print_impl.stmt_counter in
  Medium_Cache.select medium_conn "SELECT 1" 
    (fun stmt -> Medium_Cache.finish_params (Medium_Cache.start_params stmt 0)) 
    (fun _ -> ());
  let after_first = !Print_impl.stmt_counter in
  
  printf "First query cached: %s\n" (if after_first = before_first then "YES" else "NO");
  assert (after_first = before_first);
  
  let before_second = !Print_impl.stmt_counter in
  Medium_Cache.select medium_conn "SELECT 2" 
    (fun stmt -> Medium_Cache.finish_params (Medium_Cache.start_params stmt 0)) 
    (fun _ -> ());
  let after_second = !Print_impl.stmt_counter in
  
  printf "Second query evicted: %s\n" (if after_second > before_second then "YES" else "NO");
  assert (after_second > before_second);
  printf "LRU pattern test passed for MEDIUM!\n";
  
  printf "\n=== TEST: Cycling Pattern (LARGE, size=10, cycles=20) ===\n";
  let large_conn = Large_Cache.create_cached_connection () in
  Print_impl.reset_mock_stats ();
  Print_impl.clear_mock_responses ();
  
  let cache_size = 10 in
  let cycle_count = 20 in
  let total_responses = cache_size * cycle_count in
  setup_mock_responses total_responses;
  
  for cycle = 1 to cycle_count do
    for i = 1 to cache_size do
      let sql = sprintf "SELECT %d" i in
      Large_Cache.select large_conn sql 
        (fun stmt -> Large_Cache.finish_params (Large_Cache.start_params stmt 0)) 
        (fun _ -> ());
    done;
    if cycle mod 5 = 0 then
      printf "    After cycle %d: %d prepared, %s\n" cycle !Print_impl.stmt_counter (Large_Cache.cache_stats large_conn);
  done;
  
  let total_prepared = !Print_impl.stmt_counter in
  printf "Total prepared after %d cycles: %d (expected %d for first cycle only)\n" 
    cycle_count total_prepared cache_size;
  
  assert (total_prepared = cache_size);
  printf "Cycling pattern test passed for LARGE!\n";

  printf "\n=== TEST: TTL non-zero (size=2, ttl=3s) ===\n";
  let module TTL3_Config = struct let max_cache_size = 2 let ttl_seconds = Some 3.0 end in
  let module TTL3_Cache = Sqlgg_stmt_cache.Make(TTL3_Config)(Print_impl) in
  let ttl3_conn = TTL3_Cache.create_cached_connection () in
  let snapshot3 = Print_impl.snapshot_stats () in
  Print_impl.reset_mock_stats ();
  Print_impl.clear_mock_responses ();

  setup_mock_responses 1;
  TTL3_Cache.select ttl3_conn "SELECT 42"
    (fun stmt -> TTL3_Cache.finish_params (TTL3_Cache.start_params stmt 0))
    (fun _ -> ());
  Print_impl.assert_mock_stats ~expected_prepared:1 ~expected_closed:0 ~expected_open:1 ();

  setup_mock_responses 1;
  TTL3_Cache.select ttl3_conn "SELECT 42"
    (fun stmt -> TTL3_Cache.finish_params (TTL3_Cache.start_params stmt 0))
    (fun _ -> ());
  Print_impl.assert_mock_stats ~expected_prepared:1 ~expected_closed:0 ~expected_open:1 ();
  printf "TTL>0 immediate reuse did not close — OK\n";

  printf "\n=== TEST: TTL delayed expiry (size=2, ttl=0.5s) ===\n";
  let module TTL2_Config = struct let max_cache_size = 2 let ttl_seconds = Some 0.5 end in
  let module TTL2_Cache = Sqlgg_stmt_cache.Make_with_clock(Print_impl.Clock)(TTL2_Config)(Print_impl) in
  let ttl2_conn = TTL2_Cache.create_cached_connection () in
  let snapshot2 = Print_impl.snapshot_stats () in
  Print_impl.reset_mock_stats ();
  Print_impl.clear_mock_responses ();
  Print_impl.Clock.set 0.0;

  setup_mock_responses 1;
  TTL2_Cache.select ttl2_conn "SELECT 7"
    (fun stmt -> TTL2_Cache.finish_params (TTL2_Cache.start_params stmt 0))
    (fun _ -> ());
  Print_impl.assert_mock_stats ~expected_prepared:1 ~expected_closed:0 ~expected_open:1 ();

  Print_impl.Clock.travel 0.6;

  setup_mock_responses 1;
  TTL2_Cache.select ttl2_conn "SELECT 7"
    (fun stmt -> TTL2_Cache.finish_params (TTL2_Cache.start_params stmt 0))
    (fun _ -> ());
    Print_impl.assert_mock_stats ~expected_prepared:1 ~expected_closed:1 ~expected_open:0 ();
  printf "TTL=0.5s reuse after >0.5s closed cached — OK\n";
 
  setup_mock_responses 1;
  TTL2_Cache.select ttl2_conn "SELECT 7"
    (fun stmt -> TTL2_Cache.finish_params (TTL2_Cache.start_params stmt 0))
    (fun _ -> ());
  Print_impl.assert_mock_stats ~expected_prepared:2 ~expected_closed:1 ~expected_open:1 ();
  printf "TTL=0.5s re-prepared after expiry — OK\n";

  Print_impl.restore_stats snapshot2;

  printf "\n=== TEST: TTL hit/miss behavior (size=2, ttl=0.5s) ===\n";
  let module TTL2B_Config = struct let max_cache_size = 2 let ttl_seconds = Some 0.5 end in
  let module TTL2B_Cache = Sqlgg_stmt_cache.Make_with_clock(Print_impl.Clock)(TTL2B_Config)(Print_impl) in
  let ttl2b_conn = TTL2B_Cache.create_cached_connection () in
  Print_impl.reset_mock_stats ();
  Print_impl.clear_mock_responses ();
  Print_impl.Clock.set 0.0;

  let q1 = "SELECT 10" in
  let q2 = "SELECT 11" in

  setup_mock_responses 8;

  TTL2B_Cache.select ttl2b_conn q1
    (fun stmt -> TTL2B_Cache.finish_params (TTL2B_Cache.start_params stmt 0))
    (fun _ -> ());
  Print_impl.assert_mock_stats ~expected_prepared:1 ~expected_closed:0 ~expected_open:1 ();
  printf "MISS (prepare) %s — OK\n" q1;

  TTL2B_Cache.select ttl2b_conn q1
    (fun stmt -> TTL2B_Cache.finish_params (TTL2B_Cache.start_params stmt 0))
    (fun _ -> ());
  Print_impl.assert_mock_stats ~expected_prepared:1 ~expected_closed:0 ~expected_open:1 ();
  printf "HIT within TTL %s — OK\n" q1;

  TTL2B_Cache.select ttl2b_conn q2
    (fun stmt -> TTL2B_Cache.finish_params (TTL2B_Cache.start_params stmt 0))
    (fun _ -> ());
  Print_impl.assert_mock_stats ~expected_prepared:2 ~expected_closed:0 ~expected_open:2 ();
  printf "MISS (prepare) %s — OK\n" q2;

  Print_impl.Clock.travel 0.6;

  TTL2B_Cache.select ttl2b_conn q1
    (fun stmt -> TTL2B_Cache.finish_params (TTL2B_Cache.start_params stmt 0))
    (fun _ -> ());
  Print_impl.assert_mock_stats ~expected_prepared:2 ~expected_closed:1 ~expected_open:1 ();
  printf "EXPIRED-USE (closed, no prepare) %s — OK\n" q1;

  TTL2B_Cache.select ttl2b_conn q2
    (fun stmt -> TTL2B_Cache.finish_params (TTL2B_Cache.start_params stmt 0))
    (fun _ -> ());
  Print_impl.assert_mock_stats ~expected_prepared:2 ~expected_closed:2 ~expected_open:0 ();
  printf "EXPIRED-USE (closed, no prepare) %s — OK\n" q2;

  TTL2B_Cache.select ttl2b_conn q1
    (fun stmt -> TTL2B_Cache.finish_params (TTL2B_Cache.start_params stmt 0))
    (fun _ -> ());
  Print_impl.assert_mock_stats ~expected_prepared:3 ~expected_closed:2 ~expected_open:1 ();
  printf "MISS after expiry (re-prepare) %s — OK\n" q1;

  TTL2B_Cache.select ttl2b_conn q2
    (fun stmt -> TTL2B_Cache.finish_params (TTL2B_Cache.start_params stmt 0))
    (fun _ -> ());
  Print_impl.assert_mock_stats ~expected_prepared:4 ~expected_closed:2 ~expected_open:2 ();
  printf "MISS after expiry (re-prepare) %s — OK\n" q2;
 
  printf "\n=== TEST: TTL with LRU mass expiry keeps only youngest (size=10, ttl=1s) ===\n";
  let module TTL_LRU_Config = struct let max_cache_size = 10 let ttl_seconds = Some 1.0 end in
  let module TTL_LRU_Cache = Sqlgg_stmt_cache.Make_with_clock(Print_impl.Clock)(TTL_LRU_Config)(Print_impl) in
  let lru_conn = TTL_LRU_Cache.create_cached_connection () in
  Print_impl.reset_mock_stats ();
  Print_impl.clear_mock_responses ();
  Print_impl.Clock.set 0.0;

  let cache_size = 10 in
  setup_mock_responses 10;
  for i = 1 to cache_size do
    let sql = sprintf "SELECT %d" i in
    TTL_LRU_Cache.select lru_conn sql
      (fun stmt -> TTL_LRU_Cache.finish_params (TTL_LRU_Cache.start_params stmt 0))
      (fun _ -> ());
  done;
  printf "Filled LRU: %s\n" (TTL_LRU_Cache.cache_stats lru_conn);

  Print_impl.Clock.travel 10.0;

  setup_mock_responses 1;
  TTL_LRU_Cache.select lru_conn "SELECT 11"
    (fun stmt -> TTL_LRU_Cache.finish_params (TTL_LRU_Cache.start_params stmt 0))
    (fun _ -> ());
  printf "Added 11th (should evict one old): %s\n" (TTL_LRU_Cache.cache_stats lru_conn);
  Print_impl.assert_mock_stats ~expected_prepared:11 ~expected_closed:1 ~expected_open:10 ();

  let before_hit10 = !Print_impl.stmt_counter in
  setup_mock_responses 1;
  TTL_LRU_Cache.select lru_conn "SELECT 10"
    (fun stmt -> TTL_LRU_Cache.finish_params (TTL_LRU_Cache.start_params stmt 0))
    (fun _ -> ());
  let after_hit10 = !Print_impl.stmt_counter in
  printf "Hit SELECT 10: %s\n" (if after_hit10 = before_hit10 then "YES" else "NO");
  assert (after_hit10 = before_hit10);

  let before_hit11 = !Print_impl.stmt_counter in
  setup_mock_responses 1;
  TTL_LRU_Cache.select lru_conn "SELECT 11"
    (fun stmt -> TTL_LRU_Cache.finish_params (TTL_LRU_Cache.start_params stmt 0))
    (fun _ -> ());
  let after_hit11 = !Print_impl.stmt_counter in
  printf "Hit SELECT 11: %s\n" (if after_hit11 = before_hit11 then "YES" else "NO");
  assert (after_hit11 = before_hit11);

  Print_impl.Clock.travel 1.1;

  setup_mock_responses 8;
  for i = 2 to 9 do
    let sql = sprintf "SELECT %d" i in
    TTL_LRU_Cache.select lru_conn sql
      (fun stmt -> TTL_LRU_Cache.finish_params (TTL_LRU_Cache.start_params stmt 0))
      (fun _ -> ());
  done;
  setup_mock_responses 1;
  TTL_LRU_Cache.select lru_conn "SELECT 10"
    (fun stmt -> TTL_LRU_Cache.finish_params (TTL_LRU_Cache.start_params stmt 0))
    (fun _ -> ());

  printf "After expiration accesses: %s\n" (TTL_LRU_Cache.cache_stats lru_conn);
  Print_impl.assert_mock_stats ~expected_prepared:12 ~expected_closed:10 ~expected_open:2 ();
  printf "TTL+LRU mass expiry left 2 open (SELECT 10, SELECT 11) — OK\n";

  Print_impl.restore_stats snapshot3;
  printf "\n=== TEST: TTL immediate expiry (size=2, ttl=0) ===\n";
  let module TTL_Config = struct let max_cache_size = 2 let ttl_seconds = Some 0.0 end in
  let module TTL_Cache = Sqlgg_stmt_cache.Make(TTL_Config)(Print_impl) in
  let ttl_conn = TTL_Cache.create_cached_connection () in
  let snapshot = Print_impl.snapshot_stats () in
  Print_impl.reset_mock_stats ();
  Print_impl.clear_mock_responses ();

  setup_mock_responses 1;
  TTL_Cache.select ttl_conn "SELECT 1"
    (fun stmt -> TTL_Cache.finish_params (TTL_Cache.start_params stmt 0))
    (fun _ -> ());
  Print_impl.assert_mock_stats ~expected_prepared:1 ~expected_closed:0 ~expected_open:1 ();

  setup_mock_responses 1;
  TTL_Cache.select ttl_conn "SELECT 1"
    (fun stmt -> TTL_Cache.finish_params (TTL_Cache.start_params stmt 0))
    (fun _ -> ());
  Print_impl.assert_mock_stats ~expected_prepared:1 ~expected_closed:1 ~expected_open:0 ();

  setup_mock_responses 1;
  TTL_Cache.select ttl_conn "SELECT 1"
    (fun stmt -> TTL_Cache.finish_params (TTL_Cache.start_params stmt 0))
    (fun _ -> ());
  Print_impl.assert_mock_stats ~expected_prepared:2 ~expected_closed:1 ~expected_open:1 ();
  printf "TTL immediate expiry test passed!\n";

  Print_impl.restore_stats snapshot;

  printf "\n=== TEST: Eviction does not use async (sync close fix) ===\n";
  (* This test verifies that close_stmt is NOT called via async (fire-and-forget).
     The fix removes async to avoid "Commands out of sync" errors on MySQL. *)
  let module Sync_Config = struct let max_cache_size = 1 let ttl_seconds = None end in
  let module Sync_Cache = Sqlgg_stmt_cache.Make(Sync_Config)(Print_impl) in
  let sync_conn = Sync_Cache.create_cached_connection () in
  let snapshot_sync = Print_impl.snapshot_stats () in
  Print_impl.reset_mock_stats ();
  Print_impl.clear_mock_responses ();
  Print_impl.IO.reset_async_calls ();

  (* Fill the cache *)
  setup_mock_responses 1;
  Sync_Cache.select sync_conn "SELECT 1"
    (fun stmt -> Sync_Cache.finish_params (Sync_Cache.start_params stmt 0))
    (fun _ -> ());
  
  let async_before = Print_impl.IO.get_async_calls () in
  
  (* Trigger eviction by adding second entry *)
  setup_mock_responses 1;
  Sync_Cache.select sync_conn "SELECT 2"
    (fun stmt -> Sync_Cache.finish_params (Sync_Cache.start_params stmt 0))
    (fun _ -> ());
  
  let async_after = Print_impl.IO.get_async_calls () in
  printf "Async calls before eviction: %d, after: %d\n" async_before async_after;
  
  if async_after = async_before then
    printf "Eviction does NOT use async - OK (sync close)\n"
  else
    failwith (sprintf "Eviction uses async! Before: %d, After: %d. This causes 'Commands out of sync' on MySQL."
      async_before async_after);

  printf "Sync close test passed!\n";
  Print_impl.restore_stats snapshot_sync;
  
  printf "Error handling test completed!\n";
  printf "Final mock statistics:\n";
  ignore (Print_impl.get_mock_stats ())
