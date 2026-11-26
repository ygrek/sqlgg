open OUnit2
open Sqlgg_migrations

type entry = {
  reversibility : [ `Reversible | `Irreversible ];
  storage_rank : int;
}

type operation =
  | Merge_next of int
  | Merge_any of int list
  | Migrate
  | Rollback
  | Squash of int

type scenario = {
  catalog : entry list;
  operations : operation list;
}

type world = {
  merged : int list;
  squashed_through : int option;
  applied : (int * int) list;
}

type snapshot = {
  history : Planner.history;
  baseline : Migration_id.t option;
  compiled : (Migration_id.t * [ `Reversible | `Irreversible ]) list;
  stored : (Migration_id.t * int * Migration_state.t) list;
}

let property ~name ~count ~print generator law =
  name >:: fun _ ->
  try
    QCheck2.Test.check_cell_exn
      ~rand:(Random.State.make [| 42 |])
      (QCheck2.Test.make_cell ~name ~count ~print generator law)
  with QCheck2.Test.Test_fail (_, messages) ->
    assert_failure (String.concat "\n" messages)

let migration id = function
  | `Reversible -> Migration.reversible id ~apply:`Apply ~revert:`Revert
  | `Irreversible -> Migration.irreversible id ~apply:`Apply

let stored_row id batch state =
  match
    Migration_state.parse_row
      ~id:(Migration_id.to_string id)
      ~batch:(Int64.of_int batch)
      ~state:(Migration_state.to_string state)
  with
  | Ok row -> row
  | Error error -> failwith (Migration_state.error_to_string error)

let catalog_id index =
  Migration_id.parse_generated
    ~id:(string_of_int (20260101000000 + index) ^ "_m")

let compiled_indices world =
  List.filter
    (fun index ->
      match world.squashed_through with
      | Some cutoff -> index > cutoff
      | None -> true)
    world.merged

let unapplied world =
  List.filter
    (fun index -> not (List.mem_assoc index world.applied))
    (compiled_indices world)

let latest_batch world =
  List.fold_left (fun latest (_, batch) -> Int.max latest batch) 0 world.applied

let rollback_outcome scenario world =
  match world.applied with
  | [] -> `Reverts []
  | _ :: _ ->
      let latest = latest_batch world in
      let compiled = compiled_indices world in
      if
        List.exists
          (fun (index, batch) -> batch = latest && not (List.mem index compiled))
          world.applied
      then
        `Archived latest
      else
        let members =
          List.filter
            (fun index -> List.assoc_opt index world.applied = Some latest)
            compiled
        in
        (match
           List.filter
             (fun index ->
               match (List.nth scenario.catalog index).reversibility with
               | `Reversible -> false
               | `Irreversible -> true)
             members
         with
         | [] -> `Reverts (List.rev members)
         | _ :: _ as blockers -> `Irreversible blockers)

let reached scenario =
  let merge world indices =
    { world with merged = List.sort_uniq Int.compare (world.merged @ indices) }
  in
  let mergeable world =
    List.filter
      (fun index ->
        (not (List.mem index world.merged))
        &&
        match world.squashed_through with
        | Some cutoff -> index > cutoff
        | None -> true)
      (List.init (List.length scenario.catalog) Fun.id)
  in
  let advance world = function
    | Merge_next count ->
        merge
          world
          (List.filteri (fun position _ -> position < count) (mergeable world))
    | Merge_any indices ->
        merge
          world
          (List.filter (fun index -> List.mem index indices) (mergeable world))
    | Migrate ->
        (match unapplied world with
         | [] -> world
         | _ :: _ as pending ->
             let batch = latest_batch world + 1 in
             {
               world with
               applied =
                 world.applied @ List.map (fun index -> index, batch) pending;
             })
    | Rollback ->
        (match rollback_outcome scenario world with
         | `Reverts members ->
             {
               world with
               applied =
                 List.filter
                   (fun (index, _) -> not (List.mem index members))
                   world.applied;
             }
         | `Archived _ | `Irreversible _ -> world)
    | Squash index ->
        if index < List.length scenario.catalog then
          {
            world with
            squashed_through =
              Some
                (match world.squashed_through with
                 | Some cutoff -> Int.max cutoff index
                 | None -> index);
          }
        else
          world
  in
  let initial = { merged = []; squashed_through = None; applied = [] } in
  snd
    (List.fold_left
       (fun (current, seen) operation ->
         let next = advance current operation in
         next, next :: seen)
       (initial, [ initial ])
       scenario.operations)

let planner_input scenario world =
  let rank index = (List.nth scenario.catalog index).storage_rank in
  ( Option.map catalog_id world.squashed_through,
    List.map
      (fun index ->
        migration
          (catalog_id index)
          (List.nth scenario.catalog index).reversibility)
      (compiled_indices world),
    List.map
      (fun (index, batch) ->
        stored_row (catalog_id index) batch Migration_state.Applied)
      (List.sort
         (fun (left, _) (right, _) -> Int.compare (rank left) (rank right))
         world.applied) )

let scenario_generator =
  let open QCheck2.Gen in
  let* size = int_range 1 7 in
  let* catalog =
    list_size
      (return size)
      (map2
         (fun reversibility storage_rank -> { reversibility; storage_rank })
         (oneof
            [
              return `Reversible;
              return `Reversible;
              return `Reversible;
              return `Irreversible;
            ])
         (int_range 0 99))
  in
  let+ operations =
    list_size
      (int_range 0 14)
      (oneof
         [
           map (fun count -> Merge_next count) (int_range 1 3);
           map
             (fun indices -> Merge_any indices)
             (list_size (int_range 1 3) (int_range 0 6));
           return Migrate;
           return Migrate;
           return Rollback;
           map (fun index -> Squash index) (int_range 0 6);
         ])
  in
  { catalog; operations }

let print_scenario { catalog; operations } =
  String.concat
    " "
    (List.map
       (fun { reversibility; storage_rank } ->
         (match reversibility with
          | `Reversible -> "reversible@"
          | `Irreversible -> "irreversible@")
         ^ string_of_int storage_rank)
       catalog)
  ^ " | "
  ^ String.concat
      "; "
      (List.map
         (function
           | Merge_next count -> "merge next " ^ string_of_int count
           | Merge_any indices ->
               "merge " ^ String.concat "," (List.map string_of_int indices)
           | Migrate -> "migrate"
           | Rollback -> "rollback"
           | Squash index -> "squash " ^ string_of_int index)
         operations)

let raw_id_generator =
  let open QCheck2.Gen in
  pair
    (string_size ~gen:(char_range '0' '9') (int_range 1 14))
    (oneof
       [
         return None;
         map
           Option.some
           (string_size
              ~gen:(oneof [ char_range 'a' 'z'; char_range '0' '9'; return '_' ])
              (int_range 1 12));
       ])

let raw_id (digits, name) =
  match name with
  | None -> digits
  | Some name -> digits ^ "_" ^ name

let () =
  run_test_tt_main
    ("migration properties"
     >::: [
            property
              ~name:"interleaved plans follow the deployment model"
              ~count:2000
              ~print:print_scenario
              scenario_generator
              (fun scenario ->
                List.for_all
                  (fun world ->
                    let baseline, migrations, rows =
                      planner_input scenario world
                    in
                    let text index = Migration_id.to_string (catalog_id index) in
                    let planned =
                      Result.map
                        (List.map (fun (step : _ Planner.step) ->
                             ( Migration_id.to_string step.migration_id,
                               step.batch,
                               step.run )))
                    in
                    Result.map
                      (List.map Migration_id.to_string)
                      (Planner.pending
                         ?baseline
                         Planner.Interleaved
                         migrations
                         rows)
                    = Ok (List.map text (unapplied world))
                    && planned
                         (Planner.migrate
                            ?baseline
                            Planner.Interleaved
                            migrations
                            rows)
                       = Ok
                           (List.map
                              (fun index ->
                                text index, latest_batch world + 1, `Apply)
                              (unapplied world))
                    &&
                    match
                      ( rollback_outcome scenario world,
                        planned
                          (Planner.rollback
                             ?baseline
                             Planner.Interleaved
                             migrations
                             rows) )
                    with
                    | `Reverts members, Ok steps ->
                        steps
                        = List.map
                            (fun index ->
                              text index, latest_batch world, `Revert)
                            members
                    | `Archived batch, Error (`Archived_batch refused) ->
                        batch = refused
                    | `Irreversible blockers, Error (`Irreversible id) ->
                        List.exists
                          (fun index -> text index = Migration_id.to_string id)
                          blockers
                    | (`Reverts _ | `Archived _ | `Irreversible _), (Ok _ | Error _)
                      -> false)
                  (reached scenario));
            property
              ~name:"linear history accepts exactly in-order deployments"
              ~count:2000
              ~print:print_scenario
              scenario_generator
              (fun scenario ->
                List.for_all
                  (fun world ->
                    let baseline, migrations, rows =
                      planner_input scenario world
                    in
                    let compiled = compiled_indices world in
                    let rec applied_prefix = function
                      | [] -> true
                      | index :: rest when List.mem_assoc index world.applied ->
                          applied_prefix rest
                      | _ :: rest ->
                          not
                            (List.exists
                               (fun later -> List.mem_assoc later world.applied)
                               rest)
                    in
                    let rec ascending = function
                      | previous :: (current :: _ as rest) ->
                          previous <= current && ascending rest
                      | [] | [ _ ] -> true
                    in
                    let refused = function
                      | Error
                          ( `Missing_stored_id _
                          | `Missing_batch _
                          | `Reappeared_batch _
                          | `Non_monotonic_batch _ ) -> true
                      | Ok _
                      | Error
                          ( `Duplicate_id _
                          | `Out_of_order _
                          | `Before_baseline _
                          | `Unknown_stored_id _
                          | `Non_final_state _
                          | `Irreversible _
                          | `Archived_batch _
                          | `Batch_overflow ) -> false
                    in
                    if
                      applied_prefix compiled
                      && ascending
                           ((match
                               List.filter
                                 (fun (index, _) -> not (List.mem index compiled))
                                 world.applied
                             with
                             | [] -> []
                             | _ :: _ as archived ->
                                 [
                                   List.fold_left
                                     (fun latest (_, batch) ->
                                       Int.max latest batch)
                                     0
                                     archived;
                                 ])
                            @ List.filter_map
                                (fun index ->
                                  List.assoc_opt index world.applied)
                                compiled)
                    then
                      Planner.pending ?baseline Planner.Linear migrations rows
                      = Planner.pending
                          ?baseline
                          Planner.Interleaved
                          migrations
                          rows
                      && Planner.migrate ?baseline Planner.Linear migrations rows
                         = Planner.migrate
                             ?baseline
                             Planner.Interleaved
                             migrations
                             rows
                      && Planner.rollback
                           ?baseline
                           Planner.Linear
                           migrations
                           rows
                         = Planner.rollback
                             ?baseline
                             Planner.Interleaved
                             migrations
                             rows
                    else
                      refused
                        (Planner.pending
                           ?baseline
                           Planner.Linear
                           migrations
                           rows)
                      && refused
                           (Planner.migrate
                              ?baseline
                              Planner.Linear
                              migrations
                              rows)
                      && refused
                           (Planner.rollback
                              ?baseline
                              Planner.Linear
                              migrations
                              rows))
                  (reached scenario));
            property
              ~name:"an accepted plan is sound for any state table"
              ~count:5000
              ~print:(fun { history; baseline; compiled; stored } ->
                (match history with
                 | Planner.Linear -> "linear"
                 | Planner.Interleaved -> "interleaved")
                ^ " baseline="
                ^ (match baseline with
                   | Some id -> Migration_id.to_string id
                   | None -> "none")
                ^ " compiled="
                ^ String.concat
                    ","
                    (List.map
                       (fun (id, reversibility) ->
                         Migration_id.to_string id
                         ^
                         match reversibility with
                         | `Reversible -> ""
                         | `Irreversible -> "!")
                       compiled)
                ^ " stored="
                ^ String.concat
                    ","
                    (List.map
                       (fun (id, batch, state) ->
                         Migration_id.to_string id
                         ^ "#"
                         ^ string_of_int batch
                         ^ ":"
                         ^ Migration_state.to_string state)
                       stored))
              (let open QCheck2.Gen in
               let pooled_id =
                 map2
                   (fun offset name ->
                     Migration_id.parse_generated
                       ~id:(string_of_int (20260101000000 + offset) ^ name))
                   (int_range 0 5)
                   (oneof [ return ""; return "_a"; return "_b" ])
               in
               let in_position_order id_of entries =
                 List.stable_sort
                   (fun left right ->
                     Migration_id.compare_position (id_of left) (id_of right))
                   (List.fold_left
                      (fun distinct entry ->
                        if
                          List.exists
                            (fun earlier ->
                              Migration_id.to_string (id_of earlier)
                              = Migration_id.to_string (id_of entry))
                            distinct
                        then
                          distinct
                        else
                          distinct @ [ entry ])
                      []
                      entries)
               in
               let* history =
                 oneof [ return Planner.Linear; return Planner.Interleaved ]
               in
               let* baseline =
                 oneof
                   [ return None; return None; map Option.some pooled_id ]
               in
               let* compiled =
                 let* unordered =
                   list_size
                     (int_range 0 6)
                     (pair
                        pooled_id
                        (oneof
                           [
                             return `Reversible;
                             return `Reversible;
                             return `Irreversible;
                           ]))
                 in
                 let ordered = in_position_order fst unordered in
                 oneof
                   [
                     return unordered;
                     return ordered;
                     return ordered;
                     return ordered;
                   ]
               in
               let+ stored =
                 let* repeating =
                   list_size
                     (int_range 0 5)
                     (triple
                        (oneof
                           (pooled_id
                            :: List.map (fun (id, _) -> return id) compiled))
                        (int_range 1 4)
                        (oneof
                           (List.map
                              return
                              Migration_state.
                                [
                                  Applied;
                                  Applied;
                                  Applied;
                                  Applied;
                                  Applied;
                                  Applied;
                                  Applying;
                                  Reverting;
                                  Apply_failed;
                                  Revert_failed;
                                ])))
                 in
                 let distinct =
                   in_position_order (fun (id, _, _) -> id) repeating
                 in
                 oneof [ return repeating; return distinct; return distinct ]
               in
               { history; baseline; compiled; stored })
              (fun { history; baseline; compiled; stored } ->
                let migrations =
                  List.map
                    (fun (id, reversibility) -> migration id reversibility)
                    compiled
                in
                let rows =
                  List.map
                    (fun (id, batch, state) -> stored_row id batch state)
                    stored
                in
                let stored_batch id =
                  List.find_map
                    (fun (stored_id, batch, _) ->
                      if
                        Migration_id.to_string stored_id
                        = Migration_id.to_string id
                      then
                        Some batch
                      else
                        None)
                    stored
                in
                let highest =
                  List.fold_left
                    (fun highest (_, batch, _) -> Int.max highest batch)
                    0
                    stored
                in
                let settled =
                  List.for_all
                    (fun (_, _, state) ->
                      match state with
                      | Migration_state.Applied -> true
                      | Migration_state.Applying
                      | Migration_state.Reverting
                      | Migration_state.Apply_failed
                      | Migration_state.Revert_failed -> false)
                    stored
                in
                let planned =
                  List.map (fun (step : _ Planner.step) ->
                      ( Migration_id.to_string step.migration_id,
                        step.batch,
                        step.run ))
                in
                (match
                   ( Planner.pending ?baseline history migrations rows,
                     Planner.migrate ?baseline history migrations rows )
                 with
                 | Ok ids, Ok steps ->
                     settled
                     && List.for_all
                          (fun id ->
                            match baseline with
                            | Some cutoff ->
                                Migration_id.compare_position id cutoff > 0
                            | None -> true)
                          ids
                     && planned steps
                        = List.map
                            (fun id ->
                              Migration_id.to_string id, highest + 1, `Apply)
                            ids
                     && List.map Migration_id.to_string ids
                        = List.filter_map
                            (fun (id, _) ->
                              match stored_batch id with
                              | Some _ -> None
                              | None -> Some (Migration_id.to_string id))
                            compiled
                 | Error refused, Error also_refused -> refused = also_refused
                 | Ok _, Error _ | Error _, Ok _ -> false)
                &&
                match Planner.rollback ?baseline history migrations rows with
                | Error _ -> true
                | Ok steps ->
                    settled
                    && planned steps
                       = List.rev
                           (List.filter_map
                              (fun (id, _) ->
                                match stored_batch id with
                                | Some batch when batch = highest ->
                                    Some
                                      ( Migration_id.to_string id,
                                        highest,
                                        `Revert )
                                | Some _ | None -> None)
                              compiled));
            property
              ~name:"an id keeps its text, ordinal and name"
              ~count:2000
              ~print:raw_id
              raw_id_generator
              (fun (digits, name) ->
                match Migration_id.parse (raw_id (digits, name)) with
                | Error (`Invalid _ | `Too_long _) -> false
                | Ok id ->
                    Migration_id.to_string id = raw_id (digits, name)
                    && Migration_id.ordinal id = Int64.of_string digits
                    && Migration_id.name id = name
                    &&
                    (match
                       Migration_id.make
                         ~ordinal:(Migration_id.ordinal id)
                         ~name:(Migration_id.name id)
                     with
                     | Ok made ->
                         Migration_id.compare_position made id = 0
                     | Error (`Invalid _ | `Too_long _) -> false));
            property
              ~name:"the next position is the first one after an id"
              ~count:5000
              ~print:(fun (id, other) -> raw_id id ^ " " ^ raw_id other)
              (let open QCheck2.Gen in
               let* digits, name = raw_id_generator in
               let+ other =
                 oneof
                   [
                     raw_id_generator;
                     map
                       (fun finer -> digits ^ finer, None)
                       (string_size
                          ~gen:(char_range '0' '9')
                          (int_range 0 (14 - String.length digits)));
                   ]
               in
               (digits, name), other)
              (fun (id, other) ->
                let id = Migration_id.parse_generated ~id:(raw_id id) in
                let other = Migration_id.parse_generated ~id:(raw_id other) in
                match Migration_id.next_position id with
                | Error `Position_overflow -> false
                | Ok ordinal ->
                    (match Migration_id.make ~ordinal ~name:None with
                     | Error (`Invalid _ | `Too_long _) -> false
                     | Ok next ->
                         Migration_id.compare_position id next < 0
                         && (Migration_id.compare_position id other >= 0
                             || Migration_id.compare_position next other <= 0)));
          ])
