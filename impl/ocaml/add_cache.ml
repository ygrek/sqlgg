open Sqlgg_caching

module Make
  (Base : Sqlgg_traits.M)
  (Param_serializer : sig
    val serialize_param : Base.params -> string list
  end)
  (Config : sig val config : Cache_config.t end)
  : Sqlgg_traits.M_cached 
  with type statement = Base.statement
  and type 'a connection = 'a Base.connection
  and type row = Base.row
  and type result = Base.result
  and type params = Base.params
  and type execute_response = Base.execute_response =
struct

include Base

(* Cache state *)
type cache_stats = { hits: int; misses: int; evictions: int }

type cache_state = {
  select_cache: (row list) LRU_cache.t;
  execute_cache: execute_response LRU_cache.t;
  stats: cache_stats;
}

let cache_state = ref {
  select_cache = LRU_cache.create Config.config.max_entries;
  execute_cache = LRU_cache.create Config.config.max_entries;
  stats = { hits = 0; misses = 0; evictions = 0 };
}

let update_stats f =
  let current = !cache_state in
  cache_state := { current with stats = f current.stats }

let incr_hits () = update_stats (fun s -> { s with hits = s.hits + 1 })
let incr_misses () = update_stats (fun s -> { s with misses = s.misses + 1 })

let get_cache_stats () = !cache_state.stats

let clear_cache () =
  cache_state := {
    select_cache = LRU_cache.clear !cache_state.select_cache;
    execute_cache = LRU_cache.clear !cache_state.execute_cache;
    stats = { hits = 0; misses = 0; evictions = 0 };
  }

(* Tracking for caching *)
let current_sql = ref None
let current_params = ref []

let make_cache_key sql params =
  let param_strings = Array.of_list params in
  Cache_key.make sql param_strings

(* Wrapper для отслеживания вызовов *)
let wrap_set_params original_set_params sql =
  current_sql := Some sql;
  current_params := [];
  original_set_params

let track_current_call params = 
  let serialized = Param_serializer.serialize_param params in
  current_params := !current_params @ serialized



let select db sql set_params callback =
  if not Config.config.enable_select_cache then
    Base.select db sql set_params callback
  else
    (* Перехватываем параметры через wrapper *)
    let tracking_set_params stmt =
      let params = set_params stmt in
      track_current_call params;
      params
    in
    
    current_sql := Some sql;
    current_params := [];
    
    (* Создаем ключ кеша после выполнения set_params *)
    let cache_key = make_cache_key sql !current_params in
    let state = !cache_state in
    let cached_result, new_select_cache = LRU_cache.get state.select_cache cache_key in
    
    match cached_result with
    | Some rows ->
        incr_hits ();
        cache_state := { state with select_cache = new_select_cache };
        List.iter callback rows
    | None ->
        incr_misses ();
        let results = ref [] in
        let caching_callback row = 
          results := row :: !results;
          callback row 
        in
        Base.select db sql tracking_set_params caching_callback;
        let final_results = List.rev !results in
        let new_cache = LRU_cache.put state.select_cache cache_key final_results in
        cache_state := { !cache_state with select_cache = new_cache }

let select_one_maybe db sql set_params convert =
  if not Config.config.enable_select_cache then
    Base.select_one_maybe db sql set_params convert
  else
    current_sql := Some sql;
    current_params := [];
    
    let tracking_set_params stmt =
      let params = set_params stmt in
      track_current_call params;
      params
    in
    
    let cache_key = make_cache_key sql !current_params in
    let state = !cache_state in
    let cached_result, new_select_cache = LRU_cache.get state.select_cache cache_key in
    
    match cached_result with
    | Some (row :: _) ->
        incr_hits ();
        cache_state := { state with select_cache = new_select_cache };
        Some (convert row)
    | Some [] ->
        incr_hits ();
        cache_state := { state with select_cache = new_select_cache };
        None
    | None ->
        incr_misses ();
        let result = Base.select_one_maybe db sql tracking_set_params convert in
        (* Кешируем результат *)
        let final_results = match result with Some _ -> [] | None -> [] in
        let new_cache = LRU_cache.put state.select_cache cache_key final_results in
        cache_state := { !cache_state with select_cache = new_cache };
        result

let select_one db sql set_params convert =
  match select_one_maybe db sql set_params convert with
  | Some result -> result
  | None -> raise (Oops ("no row but one expected : " ^ sql))

let execute db sql set_params =
  if not Config.config.enable_execute_cache then
    Base.execute db sql set_params
  else
    Base.execute db sql set_params

end
