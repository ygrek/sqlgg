module Cache_key = struct
  type t = {
    sql: string;
    params: string array;
  }
  
  let compare_array cmp a1 a2 =
    let len1 = Array.length a1 in
    let len2 = Array.length a2 in
    if len1 <> len2 then compare len1 len2
    else
      let rec loop i =
        if i >= len1 then 0
        else
          let c = cmp a1.(i) a2.(i) in
          if c <> 0 then c else loop (i + 1)
      in
      loop 0
  
  let compare t1 t2 =
    let sql_cmp = String.compare t1.sql t2.sql in
    if sql_cmp <> 0 then sql_cmp
    else compare_array String.compare t1.params t2.params
  
  let hash t =
    let sql_hash = Hashtbl.hash t.sql in
    let params_hash = Hashtbl.hash (Array.to_list t.params) in
    Hashtbl.hash (sql_hash, params_hash)
  
  let make sql params = { sql; params }
end

(* LRU cache *)
module LRU_cache = struct
  module CacheMap = Map.Make(Cache_key)
  
  type 'a entry = {
    value: 'a;
    timestamp: float;
    access_count: int;
  }
  
  type 'a t = {
    data: 'a entry CacheMap.t;
    max_size: int;
    access_order: (Cache_key.t * float) list;
  }
  
  let create max_size = {
    data = CacheMap.empty;
    max_size;
    access_order = [];
  }
  
  let get cache key =
    match CacheMap.find_opt key cache.data with
    | None -> None, cache
    | Some entry ->
        let new_entry = { 
          entry with 
          timestamp = Unix.time ();
          access_count = entry.access_count + 1 
        } in
        let new_data = CacheMap.add key new_entry cache.data in
        let new_order = (key, new_entry.timestamp) :: 
          List.filter (fun (k, _) -> Cache_key.compare k key <> 0) cache.access_order in
        Some entry.value, { cache with data = new_data; access_order = new_order }
  
  let evict_oldest cache =
    match List.rev cache.access_order with
    | [] -> cache
    | (oldest_key, _) :: rest ->
        let new_data = CacheMap.remove oldest_key cache.data in
        { cache with data = new_data; access_order = List.rev rest }
  
  let put cache key value =
    let entry = {
      value;
      timestamp = Unix.time ();
      access_count = 1;
    } in
    let new_data = CacheMap.add key entry cache.data in
    let new_order = (key, entry.timestamp) :: cache.access_order in
    let cache' = { cache with data = new_data; access_order = new_order } in
    if CacheMap.cardinal cache'.data > cache.max_size then
      evict_oldest cache'
    else
      cache'
  
  let clear cache = { cache with data = CacheMap.empty; access_order = [] }
end

(* Cache configuration *)
module Cache_config = struct
  type t = {
    max_entries: int;
    ttl_seconds: float;
    enable_select_cache: bool;
    enable_execute_cache: bool;
  }
  
  let default = {
    max_entries = 1000;
    ttl_seconds = 300.0;
    enable_select_cache = true;
    enable_execute_cache = false;
  }
  
  let disabled = {
    max_entries = 0;
    ttl_seconds = 0.0;
    enable_select_cache = false;
    enable_execute_cache = false;
  }
end


