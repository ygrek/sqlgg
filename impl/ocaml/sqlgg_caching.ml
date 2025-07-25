open Printf

module type MUTEX = sig
  type t
  val create : unit -> t
  val with_lock : t -> (unit -> 'a) -> 'a
end

module type IO_MUTEX = sig
  type 'a io
  type t
  val create : unit -> t
  val with_lock : t -> (unit -> 'a io) -> 'a io
end

module type CACHEABLE = sig
  include Sqlgg_traits.M
  
  (* Functions for working with prepared statements *)
  val prepare : 'a connection -> string -> statement
  val close : statement -> unit
  val with_statement : 'a connection -> string -> (statement -> 'b) -> 'b
  
  (* Direct statement execution - bypasses standard API *)
  val exec_select : statement -> (statement -> result) -> (row -> unit) -> unit
  val exec_select_one : statement -> (statement -> result) -> (row -> 'a) -> 'a
  val exec_select_maybe : statement -> (statement -> result) -> (row -> 'a) -> 'a option
end

(* ===== CACHING FUNCTOR ===== *)

module Make_cached(M : MUTEX)(Base : CACHEABLE) = struct
  include Base
  
  type cache_stats = { hits: int; misses: int; evictions: int }
  
  type stmt_cache = {
    statements: (string, Base.statement) Hashtbl.t;
    mutable hits: int;
    mutable misses: int;
    max_size: int;
    mutex: M.t;
  }
  
  let create_cache max_size = {
    statements = Hashtbl.create max_size;
    hits = 0;
    misses = 0;
    max_size;
    mutex = M.create ();
  }
  
  let with_cache_lock cache f = M.with_lock cache.mutex f
  
  let rec take n lst =
    if n <= 0 then []
    else match lst with
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs
  
  let get_statement cache conn sql =
    with_cache_lock cache (fun () ->
      match Hashtbl.find_opt cache.statements sql with
      | Some stmt ->
          cache.hits <- cache.hits + 1;
          stmt
      | None ->
          cache.misses <- cache.misses + 1;
          
          (* Simple eviction when full *)
          if Hashtbl.length cache.statements >= cache.max_size then (
            let entries = Hashtbl.to_seq cache.statements |> List.of_seq in
            let to_remove = take (List.length entries / 2) entries in
            List.iter (fun (_, stmt) -> Base.close stmt) to_remove;
            List.iter (fun (sql, _) -> Hashtbl.remove cache.statements sql) to_remove
          );
          
          let stmt = Base.prepare conn sql in
          Hashtbl.add cache.statements sql stmt;
          stmt
    )
  
  type 'a cached_connection = {
    base: 'a Base.connection;
    cache: stmt_cache option;
  }
  
  let wrap ?(enabled=true) ?(size=100) base_conn = {
    base = base_conn;
    cache = if enabled then Some (create_cache size) else None;
  }
  
  let stats cached_conn =
    match cached_conn.cache with
    | Some cache -> 
        with_cache_lock cache (fun () ->
          { hits = cache.hits; misses = cache.misses; evictions = 0 })
    | None -> { hits = 0; misses = 0; evictions = 0 }
  
  let clear cached_conn =
    match cached_conn.cache with
    | Some cache -> 
        with_cache_lock cache (fun () ->
          Hashtbl.iter (fun _ stmt -> Base.close stmt) cache.statements;
          Hashtbl.clear cache.statements;
          cache.hits <- 0;
          cache.misses <- 0)
    | None -> ()
  
  (* Cached SELECT operations *)
  let select cached_conn sql set_params callback =
    match cached_conn.cache with
    | Some cache ->
        let stmt = get_statement cache cached_conn.base sql in
        Base.exec_select stmt set_params callback
    | None ->
        Base.select cached_conn.base sql set_params callback
  
  let select_one cached_conn sql set_params convert =
    match cached_conn.cache with
    | Some cache ->
        let stmt = get_statement cache cached_conn.base sql in
        Base.exec_select_one stmt set_params convert
    | None ->
        Base.select_one cached_conn.base sql set_params convert
  
  let select_one_maybe cached_conn sql set_params convert =
    match cached_conn.cache with
    | Some cache ->
        let stmt = get_statement cache cached_conn.base sql in
        Base.exec_select_maybe stmt set_params convert
    | None ->
        Base.select_one_maybe cached_conn.base sql set_params convert
  
  (* execute is never cached *)
  let execute cached_conn sql set_params =
    Base.execute cached_conn.base sql set_params
end

(* ===== IO VERSION ===== *)

module type IO_CACHEABLE = sig
  include Sqlgg_traits.M_io
  
  val prepare : 'a connection -> string -> statement IO.future
  val close : statement -> unit IO.future
  val exec_select : statement -> (statement -> result IO.future) -> (row -> unit) -> unit IO.future
  val exec_select_one : statement -> (statement -> result IO.future) -> (row -> 'a) -> 'a IO.future
  val exec_select_maybe : statement -> (statement -> result IO.future) -> (row -> 'a) -> 'a option IO.future
end

module Make_cached_io
  (IO0 : Sqlgg_io.M)
  (M : IO_MUTEX with type 'a io = 'a IO0.future)
  (Base : IO_CACHEABLE with module IO = IO0) = struct
  
  include Base
  
  type cache_stats = { hits: int; misses: int; evictions: int }
  
  type stmt_cache = {
    statements: (string, Base.statement) Hashtbl.t;
    mutable hits: int;
    mutable misses: int;
    max_size: int;
    mutex: M.t;
  }
  
  let create_cache max_size = {
    statements = Hashtbl.create max_size;
    hits = 0;
    misses = 0;
    max_size;
    mutex = M.create ();
  }
  
  let with_cache_lock cache f = M.with_lock cache.mutex f
  
  let get_statement cache conn sql =
    with_cache_lock cache (fun () ->
      match Hashtbl.find_opt cache.statements sql with
      | Some stmt ->
          cache.hits <- cache.hits + 1;
          Base.IO.return stmt
      | None ->
          cache.misses <- cache.misses + 1;
          let open Base.IO in
          Base.prepare conn sql >>= fun stmt ->
          Hashtbl.add cache.statements sql stmt;
          return stmt
    )
  
  type 'a cached_connection = {
    base: 'a Base.connection;
    cache: stmt_cache option;
  }
  
  let wrap ?(enabled=true) ?(size=100) base_conn = {
    base = base_conn;
    cache = if enabled then Some (create_cache size) else None;
  }
  
  let select cached_conn sql set_params callback =
    match cached_conn.cache with
    | Some cache ->
        let open Base.IO in
        get_statement cache cached_conn.base sql >>= fun stmt ->
        Base.exec_select stmt set_params callback
    | None ->
        Base.select cached_conn.base sql set_params callback
  
  let select_one cached_conn sql set_params convert =
    match cached_conn.cache with
    | Some cache ->
        let open Base.IO in
        get_statement cache cached_conn.base sql >>= fun stmt ->
        Base.exec_select_one stmt set_params convert
    | None ->
        Base.select_one cached_conn.base sql set_params convert
  
  let select_one_maybe cached_conn sql set_params convert =
    match cached_conn.cache with
    | Some cache ->
        let open Base.IO in
        get_statement cache cached_conn.base sql >>= fun stmt ->
        Base.exec_select_maybe stmt set_params convert
    | None ->
        Base.select_one_maybe cached_conn.base sql set_params convert
  
  let execute cached_conn sql set_params =
    Base.execute cached_conn.base sql set_params
end
