type 'run reversal =
  | Revert_with of 'run
  | No_revert

type 'run migration = {
  compiled_id : Migration_id.t;
  apply : 'run;
  reversal : 'run reversal;
}

type 'run step = {
  migration_id : Migration_id.t;
  batch : int;
  run : 'run;
}

type error = [
  | `Duplicate_id of Migration_id.t
  | `Out_of_order of Migration_id.t * Migration_id.t
  | `Unknown_stored_id of Migration_id.t
  | `Missing_stored_id of Migration_id.t
  | `Non_final_state of Migration_id.t * Migration_state.t
  | `Non_monotonic_batch of int * int
  | `Missing_batch of int
  | `Reappeared_batch of int
  | `Irreversible of Migration_id.t
  | `Batch_overflow
]

type batch_history =
  | No_applied_migrations
  | Latest_batch of int

type 'run validated = {
  applied_migrations : ('run migration * Migration_state.row) list;
  pending_migrations : 'run migration list;
  batch_history : batch_history;
}

let reversible compiled_id ~apply ~revert =
  { compiled_id; apply; reversal = Revert_with revert }

let irreversible compiled_id ~apply =
  { compiled_id; apply; reversal = No_revert }

let validate migrations (rows : Migration_state.row list) =
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
  let compiled_ids =
    List.map (fun migration -> migration.compiled_id) migrations
  in
  
  let rec first_unordered = function
    | previous :: (current :: _ as rest) ->
        if Migration_id.compare previous current > 0 then
          Some (previous, current)
        else
          first_unordered rest
    | [] | [ _ ] -> None
  in
  let compiled = Hashtbl.create 64 in
  List.iter
    (fun id -> Hashtbl.replace compiled (Migration_id.to_string id) ())
    compiled_ids;
  let stored = Hashtbl.create 64 in
  List.iter
    (fun (row : Migration_state.row) ->
      Hashtbl.replace stored (Migration_id.to_string row.migration_id) row)
    rows;
  let stored_row migration =
    Hashtbl.find_opt stored (Migration_id.to_string migration.compiled_id)
  in
  
  let rec split_applied applied = function
    | [] -> Ok (List.rev applied, [])
    | migration :: rest as remaining ->
        (match stored_row migration with
         | Some row -> split_applied ((migration, row) :: applied) rest
         | None ->
             if
               List.exists
                 (fun later -> Option.is_some (stored_row later))
                 rest
             then
               Error (`Missing_stored_id migration.compiled_id)
             else
               Ok (List.rev applied, remaining))
  in
  
  let batch_history = function
    | [] -> Ok No_applied_migrations
    | (_, (first : Migration_state.row)) :: rest ->
        let rec check current_batch = function
          | [] -> Ok (Latest_batch current_batch)
          | (_, (row : Migration_state.row)) :: remaining ->
              if
                row.batch = current_batch
                || row.batch = current_batch + 1
              then
                check row.batch remaining
              else if row.batch > current_batch then
                Error (`Missing_batch (current_batch + 1))
              else if row.batch >= first.batch then
                Error (`Reappeared_batch row.batch)
              else
                Error (`Non_monotonic_batch (current_batch, row.batch))
        in
        check first.batch rest
  in
  match
    ( first_duplicate compiled_ids,
      first_unordered compiled_ids,
      List.find_opt
        (fun (row : Migration_state.row) ->
          match row.state with
          | Migration_state.Applied -> false
          | Migration_state.Applying
          | Migration_state.Reverting
          | Migration_state.Apply_failed
          | Migration_state.Revert_failed -> true)
        rows,
      first_duplicate
        (List.map
           (fun (row : Migration_state.row) -> row.migration_id)
           rows),
      List.find_opt
        (fun (row : Migration_state.row) ->
          not
            (Hashtbl.mem compiled (Migration_id.to_string row.migration_id)))
        rows )
  with
  | Some id, _, _, _, _ -> Error (`Duplicate_id id)
  | None, Some (previous, current), _, _, _ ->
      Error (`Out_of_order (previous, current))
  | None, None, Some row, _, _ ->
      Error (`Non_final_state (row.migration_id, row.state))
  | None, None, None, Some id, _ -> Error (`Duplicate_id id)
  | None, None, None, None, Some row ->
      Error (`Unknown_stored_id row.migration_id)
  | None, None, None, None, None ->
      Result.bind
        (split_applied [] migrations)
        (fun (applied_migrations, pending_migrations) ->
          Result.map
            (fun batch_history ->
              { applied_migrations; pending_migrations; batch_history })
            (batch_history applied_migrations))

let pending migrations rows =
  Result.map
    (fun validated ->
      List.map
        (fun migration -> migration.compiled_id)
        validated.pending_migrations)
    (validate migrations rows)

let migrate migrations rows =
  match validate migrations rows with
  | Error error -> Error error
  | Ok { pending_migrations = []; _ } -> Ok []
  | Ok { pending_migrations = _ :: _ as pending_migrations; batch_history; _ } ->
      let current_batch =
        match batch_history with
        | No_applied_migrations -> 0
        | Latest_batch latest_batch -> latest_batch
      in
      if current_batch = max_int then
        Error `Batch_overflow
      else
        Ok
          (List.map
             (fun migration ->
               {
                 migration_id = migration.compiled_id;
                 batch = current_batch + 1;
                 run = migration.apply;
               })
             pending_migrations)

let rollback migrations rows =
  match validate migrations rows with
  | Error error -> Error error
  | Ok { batch_history = No_applied_migrations; _ } -> Ok []
  | Ok { batch_history = Latest_batch latest_batch; applied_migrations; _ } ->
      let rec revert_latest = function
        | (migration, (row : Migration_state.row)) :: rest
          when row.batch = latest_batch ->
            (match migration.reversal with
             | No_revert -> Error (`Irreversible migration.compiled_id)
             | Revert_with revert ->
                 Result.map
                   (fun steps ->
                     {
                       migration_id = migration.compiled_id;
                       batch = latest_batch;
                       run = revert;
                     }
                     :: steps)
                   (revert_latest rest))
        | [] | _ :: _ -> Ok []
      in
      revert_latest (List.rev applied_migrations)

let error_to_string = function
  | `Duplicate_id migration_id ->
      "duplicate migration id: "
      ^ Migration_id.to_string migration_id
  | `Out_of_order (previous, current) ->
      "migration ids are out of order: "
      ^ Migration_id.to_string previous
      ^ " before "
      ^ Migration_id.to_string current
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
  | `Batch_overflow ->
      "migration batch number cannot be incremented"
