module Async_lru = struct
  module type Evict = sig
    type 'a future
    type key
    type value
    val on_evict : value -> unit future
  end

  module Make (IO : Sqlgg_io.M_control) (Evict : Evict with type 'a future = 'a IO.future) = struct
  
    open IO

    type ('k, 'v) node = {
      mutable prev: ('k, 'v) node option;
      mutable next: ('k, 'v) node option;
      key: 'k;
      value: 'v;
    }

    type ('k, 'v) t = {
      mutable head: ('k, 'v) node option;
      mutable tail: ('k, 'v) node option;
      hash: ('k, ('k, 'v) node) Hashtbl.t;
      capacity: int;
      mutable size: int;
    }

    let create capacity = {
      head = None;
      tail = None;
      hash = Hashtbl.create capacity;
      capacity;
      size = 0;
    }

    let remove_node t node =
      match node.prev with
       | None -> t.head <- node.next
       | Some prev -> prev.next <- node.next;
      match node.next with
       | None -> t.tail <- node.prev
       | Some next -> next.prev <- node.prev;
      node.prev <- None;
      node.next <- None

    let add_to_head t node =
      match t.head with
      | None ->
          t.head <- Some node;
          t.tail <- Some node;
          node.prev <- None;
          node.next <- None
      | Some head ->
          head.prev <- Some node;
          node.next <- Some head;
          node.prev <- None;
          t.head <- Some node

    let move_to_head t node =
      remove_node t node;
      add_to_head t node

    let remove_tail t =
      Option.map (fun tail ->
        remove_node t tail;
        tail
      ) t.tail

    let add key value t =
      begin match Hashtbl.find_opt t.hash key with
       | Some existing_node ->
           remove_node t existing_node;
           Hashtbl.remove t.hash key;
           t.size <- t.size - 1;
           Evict.on_evict existing_node.value >>= fun () ->
           IO.return ()
       | None -> IO.return ()
      end >>= fun () ->

      let new_node = { prev = None; next = None; key; value } in
      
      begin if t.size >= t.capacity then (
        match remove_tail t with
        | Some evicted_node ->
            Hashtbl.remove t.hash evicted_node.key;
            t.size <- t.size - 1;
            Evict.on_evict evicted_node.value
        | None -> IO.return ()
      ) else IO.return ()
      end >>= fun () ->

      add_to_head t new_node;
      Hashtbl.replace t.hash key new_node;
      t.size <- t.size + 1;
      IO.return ()

    let find key t =
      key |> Hashtbl.find_opt t.hash |> Option.map (fun node ->
        move_to_head t node;
        node.value
      )

    let remove key t =
      match Hashtbl.find_opt t.hash key with
      | Some node ->
          remove_node t node;
          Hashtbl.remove t.hash key;
          t.size <- t.size - 1;
          Evict.on_evict node.value >>= fun () ->
          IO.return true
      | None -> IO.return false

    let size t = t.size
      
    let capacity t = t.capacity
    let is_empty t = t.size = 0

    let clear t =
      
      let all_values = Hashtbl.fold (fun _ node acc -> node.value :: acc) t.hash [] in
      
      Hashtbl.clear t.hash;
      t.head <- None;
      t.tail <- None;
      t.size <- 0;
      
      List.iter_s Evict.on_evict all_values
  end
end

module type Cached_m = sig
  include Sqlgg_traits.M_control_io


  val prepare : 'a connection -> string -> statement IO.future
  val close_stmt : statement -> unit IO.future

  val select_with_stmt :
    statement -> (statement -> result IO.future) -> (row -> unit) -> unit IO.future

  val select_one_with_stmt :
    statement -> (statement -> result IO.future) -> (row -> 'a) -> 'a IO.future

  val select_one_maybe_with_stmt :
    statement -> (statement -> result IO.future) -> (row -> 'a) -> 'a option IO.future

  val execute_with_stmt :
    statement -> (statement -> result IO.future) -> execute_response IO.future
end

module type Cache_config = sig
  val max_cache_size : int
  val ttl_seconds : float option
end

module type Clock = sig
  val now_s : unit -> float
end

module Make_with_clock (Clock : Clock) (Config : Cache_config) (Impl : Cached_m) = struct
  include Impl

  type cache_entry = {
    stmt : Impl.statement;
    mutable last_use_s : float;
  }

  module Evict = struct
    type 'a future = 'a Impl.IO.future
    type key = string
    type value = cache_entry
    let on_evict (entry : cache_entry) =
      let open Impl.IO in
      catch
        (fun () -> Impl.close_stmt entry.stmt)
        (fun _ -> return ())
  end

  module LRU = Async_lru.Make(Impl.IO)(Evict)

  type cache_state = {
    mutable cache : (string, cache_entry) LRU.t;
    mutable ops_count : int;
  }

  type -'a connection = {
    original : 'a Impl.connection;
    mutable state : cache_state;
  }

  let now_s = Clock.now_s

  let is_expired entry =
    match Config.ttl_seconds with
    | None -> false
    | Some ttl -> (now_s () -. entry.last_use_s) >= ttl

  let create_cached_connection original =
    if Config.max_cache_size <= 0 then
      invalid_arg "max_cache_size must be positive";
    
    let cache = LRU.create Config.max_cache_size in
    let state = { cache; ops_count = 0 } in
    { original; state }

  let with_prepared_stmt cached_conn sql f =
    let open Impl.IO in
    let state = cached_conn.state in
    state.ops_count <- succ state.ops_count;

    let touch entry = entry.last_use_s <- now_s () in

    let with_stmt_cleanup ?(on_success=(fun () -> return ())) stmt =
      try_bind
        (fun () -> f stmt)
        (fun result -> on_success () >>= fun () -> return result)
        (fun exn ->
          LRU.remove sql state.cache >>= fun _ ->
          raise exn)
    in


    begin match LRU.find sql state.cache with
    | Some entry when not (is_expired entry) ->
        touch entry;
        with_stmt_cleanup entry.stmt
    | Some entry ->
        with_stmt_cleanup
          ~on_success:(fun () ->
            LRU.remove sql state.cache >>= fun _ -> return ())
          entry.stmt
    | None -> 
        Impl.prepare cached_conn.original sql >>= fun stmt ->
        let entry = { stmt; last_use_s = now_s () } in
        LRU.add sql entry state.cache >>= fun () ->
        with_stmt_cleanup stmt
    end

  let select cached_conn sql set_params callback =
    with_prepared_stmt cached_conn sql (fun stmt ->
      Impl.select_with_stmt stmt set_params callback)

  let select_one cached_conn sql set_params convert =
    with_prepared_stmt cached_conn sql (fun stmt ->
      Impl.select_one_with_stmt stmt set_params convert)

  let select_one_maybe cached_conn sql set_params convert =
    with_prepared_stmt cached_conn sql (fun stmt ->
      Impl.select_one_maybe_with_stmt stmt set_params convert)

  let execute cached_conn sql set_params =
    with_prepared_stmt cached_conn sql (fun stmt ->
      Impl.execute_with_stmt stmt set_params)

  let cache_stats cached_conn =
    let state = cached_conn.state in
    Printf.sprintf "Cache: %d/%d items, %d ops since start" 
      (LRU.size state.cache)
      (LRU.capacity state.cache)
      state.ops_count

  let reset_metrics cached_conn =
    let state = cached_conn.state in
    state.ops_count <- 0

  let clear_cache cached_conn =
    let state = cached_conn.state in
    LRU.clear state.cache

  let close_cached_connection ?(close_underlying=(fun _ -> Impl.IO.return ())) cached_conn =
    let open Impl.IO in
    LRU.clear cached_conn.state.cache >>= fun () ->
    close_underlying cached_conn.original
end

module Make (Config : Cache_config) (Impl : Cached_m) =
  Make_with_clock(struct let now_s = Unix.gettimeofday end)(Config)(Impl)

module type Mutex_intf = sig
  type 'a future
  type t
  val create : unit -> t
  val with_lock : t -> (unit -> 'a future) -> 'a future
end


module SharedConnectionCache (MutexImpl : Mutex_intf) (Config : Cache_config) 
  (Impl : Cached_m with type 'a IO.future = 'a MutexImpl.future)  = struct

  module Stmt_cache = Make(Config)(Impl)

  include Stmt_cache

  type nonrec 'a connection = {
    base_conn : 'a connection;
    mutex : MutexImpl.t;
  }
  
  let create_cached_connection original =
    { 
      base_conn = create_cached_connection original;
      mutex = MutexImpl.create ();
    }
  
  let select conn sql set_params callback =
    MutexImpl.with_lock conn.mutex (fun () -> 
      select conn.base_conn sql set_params callback)
    
  let select_one conn sql set_params convert =
    MutexImpl.with_lock conn.mutex (fun () -> 
      select_one conn.base_conn sql set_params convert)
    
  let select_one_maybe conn sql set_params convert =
    MutexImpl.with_lock conn.mutex (fun () -> 
      select_one_maybe conn.base_conn sql set_params convert)
    
  let execute conn sql set_params =
    MutexImpl.with_lock conn.mutex (fun () -> 
      execute conn.base_conn sql set_params)

  let cache_stats conn =  cache_stats conn.base_conn

  let clear_cache conn =
    MutexImpl.with_lock conn.mutex (fun () -> 
      clear_cache conn.base_conn)

  let close_cached_connection ?close_underlying conn =
    MutexImpl.with_lock conn.mutex (fun () -> 
      close_cached_connection ?close_underlying conn.base_conn)
end
