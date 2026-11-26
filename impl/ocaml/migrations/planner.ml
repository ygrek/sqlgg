type 'run step = {
  migration_id : Migration_id.t;
  batch : int;
  run : 'run;
}

type error = [
  | `Duplicate_id of Migration_id.t
  | `Out_of_order of Migration_id.t * Migration_id.t
  | `Before_baseline of Migration_id.t
  | `Unknown_stored_id of Migration_id.t
  | `Missing_stored_id of Migration_id.t
  | `Non_final_state of Migration_id.t * Migration_state.t
  | `Non_monotonic_batch of int * int
  | `Missing_batch of int
  | `Reappeared_batch of int
  | `Irreversible of Migration_id.t
  | `Archived_batch of int
  | `Batch_overflow
]

type history =
  | Linear
  | Interleaved

type latest_batch =
  | No_batch
  | Compiled_batch of int
  | Baseline_batch of int

type 'run validated = {
  applied_migrations : ('run Migration.t * int) list;
  pending_migrations : 'run Migration.t list;
  latest_batch : latest_batch;
}

let validate ?baseline history migrations (rows : Migration_state.row list) =
  let first_duplicate ids =
    let seen = Hashtbl.create 64 in
    List.find_opt
      (fun id ->
        let text = Migration_id.to_string id in
        if Hashtbl.mem seen text then
          true
        else (
          Hashtbl.add seen text ();
          false))
      ids
  in
  let compiled_ids = List.map Migration.id migrations in
  let rec first_unordered = function
    | previous :: (current :: _ as rest) ->
        if Migration_id.compare_position previous current > 0 then
          Some (previous, current)
        else
          first_unordered rest
    | [] | [ _ ] -> None
  in
  let compiled = Hashtbl.create 64 in
  List.iter
    (fun id -> Hashtbl.replace compiled (Migration_id.to_string id) ())
    compiled_ids;
  let archived (id : Migration_id.t) =
    match baseline with
    | Some cutoff -> Migration_id.compare_position id cutoff <= 0
    | None -> false
  in
  let archived_rows, unknown_rows =
    List.partition
      (fun (row : Migration_state.row) -> archived row.migration_id)
      (List.filter
         (fun (row : Migration_state.row) ->
           not (Hashtbl.mem compiled (Migration_id.to_string row.migration_id)))
         rows)
  in
  let stored = Hashtbl.create 64 in
  List.iter
    (fun (row : Migration_state.row) ->
      Hashtbl.replace stored (Migration_id.to_string row.migration_id) row.batch)
    rows;
  let stored_batch migration =
    Hashtbl.find_opt stored (Migration_id.to_string (Migration.id migration))
  in
  let reject make_error = function
    | Some value -> Error (make_error value)
    | None -> Ok ()
  in
  let ( let* ) result continue =
    match result with
    | Ok value -> continue value
    | Error error -> Error error
  in
  let* () =
    reject (fun id -> `Duplicate_id id) (first_duplicate compiled_ids)
  in
  let* () =
    reject
      (fun (previous, current) -> `Out_of_order (previous, current))
      (first_unordered compiled_ids)
  in
  let* () =
    reject
      (fun id -> `Before_baseline id)
      (List.find_opt archived compiled_ids)
  in
  let* () =
    reject
      (fun (row : Migration_state.row) ->
        `Non_final_state (row.migration_id, row.state))
      (List.find_opt
         (fun (row : Migration_state.row) ->
           match row.state with
           | Migration_state.Applied -> false
           | Migration_state.Applying
           | Migration_state.Reverting
           | Migration_state.Apply_failed
           | Migration_state.Revert_failed -> true)
         rows)
  in
  let* () =
    reject
      (fun id -> `Duplicate_id id)
      (first_duplicate
         (List.map
            (fun (row : Migration_state.row) -> row.migration_id)
            rows))
  in
  let* () =
    match unknown_rows with
    | row :: _ -> Error (`Unknown_stored_id row.migration_id)
    | [] -> Ok ()
  in
  let archived_max =
    List.fold_left
      (fun acc (row : Migration_state.row) ->
        match acc with
        | None -> Some row.batch
        | Some batch -> Some (Int.max batch row.batch))
      None
      archived_rows
  in
  let latest_of batch =
    match archived_max with
    | Some archived when archived = batch -> Baseline_batch batch
    | Some _ | None -> Compiled_batch batch
  in
  let applied_migrations, pending_migrations =
    List.partition_map
      (fun migration ->
        match stored_batch migration with
        | Some batch -> Either.Left (migration, batch)
        | None -> Either.Right migration)
      migrations
  in
  Result.map
    (fun latest_batch ->
      { applied_migrations; pending_migrations; latest_batch })
    (match history, applied_migrations with
     | (Linear | Interleaved), [] ->
         Ok
           (match archived_max with
            | None -> No_batch
            | Some batch -> latest_of batch)
     | Interleaved, (_, first) :: rest ->
         let compiled_max =
           List.fold_left
             (fun latest (_, batch) -> Int.max latest batch)
             first
             rest
         in
         Ok
           (latest_of
              (match archived_max with
               | None -> compiled_max
               | Some batch -> Int.max batch compiled_max))
     | Linear, (_, first) :: rest ->
         let rec first_gap = function
           | [] -> None
           | migration :: following ->
               (match stored_batch migration with
                | Some _ -> first_gap following
                | None ->
                    if
                      List.exists
                        (fun later -> Option.is_some (stored_batch later))
                        following
                    then
                      Some (Migration.id migration)
                    else
                      None)
         in
         let floor_batch =
           match archived_rows with
           | [] -> first
           | r0 :: rs ->
               List.fold_left
                 (fun minimum (row : Migration_state.row) ->
                   Int.min minimum row.batch)
                 r0.batch
                 rs
         in
         let rec check current_batch = function
           | [] -> Ok (latest_of current_batch)
           | (_, batch) :: remaining ->
               if batch = current_batch || batch = current_batch + 1 then
                 check batch remaining
               else if batch > current_batch then
                 Error (`Missing_batch (current_batch + 1))
               else if batch >= floor_batch then
                 Error (`Reappeared_batch batch)
               else
                 Error (`Non_monotonic_batch (current_batch, batch))
         in
         (match first_gap migrations with
          | Some id -> Error (`Missing_stored_id id)
          | None ->
              (match archived_max with
               | None -> check first rest
               | Some previous -> check previous applied_migrations)))

let pending ?baseline history migrations rows =
  Result.map
    (fun validated -> List.map Migration.id validated.pending_migrations)
    (validate ?baseline history migrations rows)

let migrate ?baseline history migrations rows =
  match validate ?baseline history migrations rows with
  | Error error -> Error error
  | Ok { pending_migrations = []; _ } -> Ok []
  | Ok { pending_migrations = _ :: _ as pending_migrations; latest_batch; _ } ->
      let current_batch =
        match latest_batch with
        | No_batch -> 0
        | Compiled_batch batch | Baseline_batch batch -> batch
      in
      if current_batch = max_int then
        Error `Batch_overflow
      else
        Ok
          (List.map
             (function
               | Migration.Reversible { id; apply; _ }
               | Migration.Irreversible { id; apply } ->
                   {
                     migration_id = id;
                     batch = current_batch + 1;
                     run = apply;
                   })
             pending_migrations)

let rollback ?baseline history migrations rows =
  match validate ?baseline history migrations rows with
  | Error error -> Error error
  | Ok { latest_batch = No_batch; _ } -> Ok []
  | Ok { latest_batch = Baseline_batch batch; _ } ->
      Error (`Archived_batch batch)
  | Ok { latest_batch = Compiled_batch latest_batch; applied_migrations; _ } ->
      List.fold_left
        (fun steps (migration, batch) ->
          if batch <> latest_batch then
            steps
          else
            match migration with
            | Migration.Irreversible { id; _ } ->
                Error (`Irreversible id)
            | Migration.Reversible { id; revert; _ } ->
                Result.map
                  (fun steps ->
                    {
                      migration_id = id;
                      batch = latest_batch;
                      run = revert;
                    }
                    :: steps)
                  steps)
        (Ok [])
        applied_migrations

let error_to_string = function
  | `Duplicate_id migration_id ->
      "duplicate migration id: "
      ^ Migration_id.to_string migration_id
  | `Out_of_order (previous, current) ->
      "migration ids are out of order: "
      ^ Migration_id.to_string previous
      ^ " before "
      ^ Migration_id.to_string current
  | `Before_baseline migration_id ->
      "compiled migration is not after baseline: "
      ^ Migration_id.to_string migration_id
  | `Unknown_stored_id migration_id ->
      "stored migration is not compiled: "
      ^ Migration_id.to_string migration_id
  | `Missing_stored_id migration_id ->
      "stored migrations are missing: "
      ^ Migration_id.to_string migration_id
  | `Non_final_state (migration_id, state) ->
      "migration is in non-final state "
      ^ Migration_state.to_string state
      ^ ": "
      ^ Migration_id.to_string migration_id
  | `Non_monotonic_batch (previous, current) ->
      "migration batch decreases from "
      ^ string_of_int previous
      ^ " to "
      ^ string_of_int current
  | `Missing_batch batch ->
      "migration batch history is missing batch "
      ^ string_of_int batch
  | `Reappeared_batch batch ->
      "migration batch reappears after a later batch: "
      ^ string_of_int batch
  | `Irreversible migration_id ->
      "migration is irreversible: "
      ^ Migration_id.to_string migration_id
  | `Archived_batch batch ->
      "migration batch is archived in the baseline: "
      ^ string_of_int batch
  | `Batch_overflow ->
      "migration batch number cannot be incremented"
