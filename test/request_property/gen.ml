
type kind =
  | Int
  | Uint64
  | Double
  | Decimal
  | Text
  | Blob
  | Bool
  | Datetime
  | Json
  | Enum of (string * string) list

type column = { name : string; sql_type : string; kind : kind; nullable : bool }

type columns = { first : column; rest : column list }

type table = { table_name : string; primary_key : bool; columns : columns }

let columns table = table.columns.first :: table.columns.rest

let choose state items = List.nth items (Random.State.int state (List.length items))

let enum_values =
  [ "red", "Red"; "green", "Green"; "in progress", "In_progress"; "on-hold", "On_hold";
    "404", "Num_404"; "Done", "Done"; "x", "X"; "a_b", "A_b" ]

let kinds state =
  [ Int, choose state [ "INT"; "BIGINT" ];
    Uint64, "BIGINT UNSIGNED";
    Double, choose state [ "DOUBLE"; "FLOAT" ];
    Decimal, "DECIMAL(10,2)";
    Text, choose state [ "VARCHAR(40)"; "TEXT" ];
    Blob, "BLOB";
    Bool, "BOOLEAN";
    Datetime, choose state [ "DATETIME"; "TIMESTAMP" ];
    Json, "JSON" ]

let enum_column state =
  let values =
    List.filter (fun _ -> Random.State.int state 3 > 0) enum_values
    |> function [] -> [ List.hd enum_values ] | values -> values
  in
  ( Enum values,
    Printf.sprintf "ENUM(%s)"
      (String.concat "," (List.map (fun (value, _) -> Printf.sprintf "'%s'" value) values)) )

let random_column state index =
  let kind, sql_type =
    if Random.State.int state 10 = 0 then enum_column state else choose state (kinds state)
  in
  { name = Printf.sprintf "c%d" index; sql_type; kind; nullable = Random.State.bool state }

let random_table state index =
  let primary_key = Random.State.bool state in
  let width = 1 + Random.State.int state 12 in
  let column position =
    if primary_key && position = 0 then
      { name = "c1"; sql_type = "INT"; kind = Int; nullable = false }
    else random_column state (position + 1)
  in
  let columns =
    let first = column 0 in
    let rest = List.init (width - 1) (fun position -> column (position + 1)) in
    { first; rest }
  in
  { table_name = Printf.sprintf "t%d" index; primary_key; columns }

let kitchen_sink state =
  let kinds = kinds state @ [ enum_column state ] in
  let columns =
    List.concat_map (fun (kind, sql_type) -> [ kind, sql_type, false; kind, sql_type, true ]) kinds
    |> List.mapi (fun index (kind, sql_type, nullable) ->
           { name = Printf.sprintf "c%d" (index + 1); sql_type; kind; nullable })
  in
  match columns with
  | first :: rest ->
    { table_name = "kitchen_sink"; primary_key = false; columns = { first; rest } }
  | [] -> assert false

let ddl_column table column =
  let key = table.primary_key && column.name = "c1" in
  Printf.sprintf "%s %s%s" column.name column.sql_type
    (if key then " PRIMARY KEY" else if column.nullable then "" else " NOT NULL")

let ddl table =
  Printf.sprintf "CREATE TABLE %s (%s);" table.table_name
    (String.concat ", " (List.map (ddl_column table) (columns table)))

let base_type = function
  | Int -> "'t Sqlgg_request.int_of"
  | Uint64 -> "Unsigned.UInt64.t"
  | Double -> "'t Sqlgg_request.float_of"
  | Decimal -> "'t Sqlgg_request.decimal_of"
  | Text -> "'t Sqlgg_request.text_of"
  | Blob -> "string"
  | Datetime -> "'t Sqlgg_request.datetime_of"
  | Bool -> "'t Sqlgg_request.bool_of"
  | Json -> "'t Sqlgg_request.json_of"
  | Enum values ->
    Printf.sprintf "[ %s ]"
      (String.concat " | " (List.map (fun (_, variant) -> "`" ^ variant) values))

let family = function
  | Int -> "Int"
  | Uint64 -> "UInt64"
  | Double -> "Float"
  | Decimal -> "Decimal"
  | Text | Blob | Datetime -> "String"
  | Bool -> "Bool"
  | Json -> "Json"
  | Enum _ -> "Enum"

let families columns =
  String.concat "; "
    (List.map
       (fun column ->
         Printf.sprintf "(%S, Check.%s)" (family column.kind)
           (if column.nullable then "Nullable" else "Required"))
       columns)

let ocaml_type column =
  if column.nullable then base_type column.kind ^ " option" else base_type column.kind

let row_type columns =
  match columns with
  | [ column ] -> ocaml_type column
  | columns -> String.concat " * " (List.map ocaml_type columns)

let arguments columns =
  String.concat ""
    (List.map (fun column -> Printf.sprintf "%s:%s -> " column.name (ocaml_type column)) columns)

let names columns = String.concat ", " (List.map (fun column -> column.name) columns)

let parameters columns =
  String.concat ", " (List.map (fun column -> "@" ^ column.name) columns)

let tuple_pattern columns =
  match columns with
  | [ column ] -> column.name
  | columns -> names columns

let labelled columns =
  String.concat " " (List.map (fun column -> "~" ^ column.name) columns)

let text state =
  let pieces = [ ""; "a"; "it's"; "say \"hi\""; "back\\slash"; "line\nbreak"; "nul\000byte"; "é"; "%s ? @x" ] in
  String.concat "" (List.init (Random.State.int state 3) (fun _ -> choose state pieces))

let rec json state depth =
  match Random.State.int state (if depth = 0 then 4 else 6) with
  | 0 -> "`Null"
  | 1 -> Printf.sprintf "`Bool %b" (Random.State.bool state)
  | 2 -> Printf.sprintf "`Int (%d)" (Random.State.int state 200 - 100)
  | 3 -> Printf.sprintf "`String %S" (text state)
  | 4 ->
    Printf.sprintf "`List [%s]"
      (String.concat "; " (List.init (Random.State.int state 3) (fun _ -> json state (depth - 1))))
  | _ ->
    Printf.sprintf "`Assoc [%s]"
      (String.concat "; "
         (List.init (Random.State.int state 3) (fun index ->
              Printf.sprintf "(%S, %s)" (Printf.sprintf "k%d" index) (json state (depth - 1)))))

let scalar state = function
  | Int ->
    choose state
      [ Printf.sprintf "(%dL)" (Random.State.int state 2000 - 1000); "Int64.max_int"; "Int64.min_int" ]
  | Uint64 ->
    choose state
      [ "(Unsigned.UInt64.of_string \"0\")"; "(Unsigned.UInt64.of_string \"18446744073709551615\")";
        Printf.sprintf "(Unsigned.UInt64.of_string \"%d\")" (Random.State.int state 100000) ]
  | Double | Decimal -> Printf.sprintf "(%F)" (float_of_int (Random.State.int state 4000 - 2000) /. 4.)
  | Text | Blob -> Printf.sprintf "%S" (text state)
  | Bool -> string_of_bool (Random.State.bool state)
  | Datetime ->
    Printf.sprintf "%S"
      (Printf.sprintf "20%02d-%02d-%02d %02d:%02d:%02d" (Random.State.int state 30)
         (1 + Random.State.int state 12) (1 + Random.State.int state 28) (Random.State.int state 24)
         (Random.State.int state 60) (Random.State.int state 60))
  | Json -> Printf.sprintf "(%s)" (json state 2)
  | Enum values -> "`" ^ snd (choose state values)

let value state column =
  if column.nullable && Random.State.int state 3 = 0 then "None"
  else if column.nullable then Printf.sprintf "(Some %s)" (scalar state column.kind)
  else scalar state column.kind

let write path text =
  let channel = open_out_bin path in
  output_string channel text;
  close_out channel

let implementation ~partner table =
  let columns = columns table in
  let name = table.table_name in
  let insert =
    Printf.sprintf
      "let insert_%s = [%%sql.Request \"INSERT INTO %s (%s) VALUES (%s)\"]\n" name name
      (names columns) (parameters columns)
  in
  let select =
    Printf.sprintf "let select_%s = [%%sql.Request \"SELECT %s FROM %s\"]\n" name (names columns) name
  in
  let lookup =
    if table.primary_key then
      Printf.sprintf "let lookup_%s = [%%sql.Request \"SELECT %s FROM %s WHERE c1 = @c1\"]\n" name
        (names columns) name
    else ""
  in
  let insert_many =
    Printf.sprintf "let insert_many_%s = [%%sql.Request \"INSERT INTO %s (%s) VALUES @rows\"]\n"
      name name (names columns)
  in
  let lookup_in =
    if table.primary_key then
      Printf.sprintf "let lookup_in_%s = [%%sql.Request \"SELECT %s FROM %s WHERE c1 IN @ids\"]\n"
        name (names columns) name
    else ""
  in
  let first = table.columns.first in
  let pick =
    Printf.sprintf
      "let pick_%s = [%%sql.Request \"SELECT %s FROM %s WHERE @pick { By_first { %s = @%s } | Everything { TRUE } }\"]\n"
      name (names columns) name first.name first.name
  in
  let maybe =
    Printf.sprintf
      "let maybe_%s = [%%sql.Request \"SELECT %s FROM %s WHERE @f { Some { %s = @%s } | None { TRUE } }\"]\n"
      name (names columns) name first.name first.name
  in
  let cols = names columns in
  let f = first.name in
  let nested =
    String.concat ""
      [ Printf.sprintf "let cte_%s = [%%sql.Request \"WITH x AS (SELECT %s FROM %s) SELECT %s FROM x\"]\n"
          name cols name cols;
        Printf.sprintf
          "let cte2_%s = [%%sql.Request \"WITH x AS (SELECT %s FROM %s), y AS (SELECT %s FROM x) SELECT %s FROM y\"]\n"
          name cols name cols cols;
        Printf.sprintf "let sub_from_%s = [%%sql.Request \"SELECT %s FROM (SELECT %s FROM %s) AS s\"]\n"
          name cols cols name;
        Printf.sprintf
          "let in_sub_%s = [%%sql.Request \"SELECT %s FROM %s WHERE %s IN (SELECT %s FROM %s WHERE %s = @%s)\"]\n"
          name cols name f f name f f;
        Printf.sprintf
          "let cte_two_%s = [%%sql.Request \"WITH x AS (SELECT %s FROM %s WHERE %s = @lo) SELECT %s FROM x WHERE %s = @hi\"]\n"
          name cols name f cols f;
        Printf.sprintf "let union_%s = [%%sql.Request \"SELECT %s FROM %s UNION ALL SELECT %s FROM %s\"]\n"
          name cols name cols name;
        Printf.sprintf
          "let pick_sub_%s = [%%sql.Request \"SELECT %s FROM %s WHERE @pick { By_first { %s IN (SELECT %s FROM %s WHERE %s = @%s) } | Everything { TRUE } }\"]\n"
          name cols name f f name f f;
        Printf.sprintf
          "let exists_%s = [%%sql.Request \"SELECT %s FROM %s WHERE EXISTS (SELECT 1 FROM %s u WHERE u.%s = %s.%s)\"]\n"
          name cols name name f name f ]
  in
  let update =
    Printf.sprintf "let update_%s = [%%sql.Request \"UPDATE %s SET %s = @set_v WHERE %s = @where_v\"]\n"
      name name f f
  in
  let delete_pick =
    Printf.sprintf
      "let delete_pick_%s = [%%sql.Request \"DELETE FROM %s WHERE @pick { By_first { %s = @%s } | Everything { TRUE } }\"]\n"
      name name f f
  in
  let join_q =
    let tcols =
      String.concat ", " (List.map (fun c -> Printf.sprintf "%s.%s" name c.name) columns)
    in
    Printf.sprintf
      "let join_%s = [%%sql.Request \"SELECT %s FROM %s JOIN %s ON %s.%s = @other\"]\n"
      name tcols name partner.table_name partner.table_name
      partner.columns.first.name
  in
  let xsub =
    Printf.sprintf
      "let xsub_%s = [%%sql.Request \"SELECT %s FROM %s WHERE EXISTS (SELECT 1 FROM %s WHERE %s = @other)\"]\n"
      name cols name partner.table_name partner.columns.first.name
  in
  insert ^ select ^ lookup ^ insert_many ^ lookup_in ^ pick ^ maybe ^ nested ^ update
  ^ delete_pick ^ join_q ^ xsub

let interface ~partner table =
  let columns = columns table in
  let name = table.table_name in
  let row = row_type columns in
  let lookups =
    if table.primary_key then
      Printf.sprintf
        "val lookup_%s : c1:'t Sqlgg_request.int_of -> (%s, 't, Sqlgg_request.zero_or_one) Sqlgg_request.t\n\
         val lookup_in_%s : ids:'t Sqlgg_request.int_of list -> (%s, 't, Sqlgg_request.many) Sqlgg_request.t\n"
        name row name row
    else ""
  in
  let first = table.columns.first in
  let strict_first_row =
    match table.columns.rest with
    | [] -> base_type first.kind
    | rest ->
      String.concat " * " (base_type first.kind :: List.map ocaml_type rest)
  in
  let cte_two_cardinality = if table.primary_key then "zero_or_one" else "many" in
  Printf.sprintf
    "val insert_%s : %s(unit, 't, Sqlgg_request.affected_rows) Sqlgg_request.t\n\
     val select_%s : (%s, 't, Sqlgg_request.many) Sqlgg_request.t\n\
     val insert_many_%s : rows:(%s) list -> (unit, 't, Sqlgg_request.affected_rows) Sqlgg_request.t\n\
     val pick_%s : pick:[ `By_first of %s | `Everything ] -> (%s, 't, Sqlgg_request.many) Sqlgg_request.t\n\
     val maybe_%s : f:[ `Some of %s | `None ] -> (%s, 't, Sqlgg_request.many) Sqlgg_request.t\n\
     val cte_%s : (%s, 't, Sqlgg_request.many) Sqlgg_request.t\n\
     val cte2_%s : (%s, 't, Sqlgg_request.many) Sqlgg_request.t\n\
     val sub_from_%s : (%s, 't, Sqlgg_request.many) Sqlgg_request.t\n\
     val in_sub_%s : %s:%s -> (%s, 't, Sqlgg_request.many) Sqlgg_request.t\n\
     val cte_two_%s : lo:%s -> hi:%s -> (%s, 't, Sqlgg_request.%s) Sqlgg_request.t\n\
     val union_%s : (%s, 't, Sqlgg_request.many) Sqlgg_request.t\n\
     val pick_sub_%s : pick:[ `By_first of %s | `Everything ] -> (%s, 't, Sqlgg_request.many) Sqlgg_request.t\n\
     val exists_%s : (%s, 't, Sqlgg_request.many) Sqlgg_request.t\n\
     val update_%s : set_v:%s -> where_v:%s -> (unit, 't, Sqlgg_request.affected_rows) Sqlgg_request.t\n\
     val delete_pick_%s : pick:[ `By_first of %s | `Everything ] -> (unit, 't, Sqlgg_request.affected_rows) Sqlgg_request.t\n\
     val join_%s : other:%s -> (%s, 't, Sqlgg_request.many) Sqlgg_request.t\n\
     val xsub_%s : other:%s -> (%s, 't, Sqlgg_request.many) Sqlgg_request.t\n\
     %s"
    name (arguments columns) name row name row name (base_type first.kind) row name
    (base_type first.kind) row name row name row name row name first.name
    (base_type first.kind) row name (base_type first.kind) (base_type first.kind)
    strict_first_row cte_two_cardinality name row name (base_type first.kind) row
    name row name (ocaml_type first) (base_type first.kind) name
    (base_type first.kind) name (base_type partner.columns.first.kind) row name
    (base_type partner.columns.first.kind) row lookups

let rows_of state table =
  let count = 1 + Random.State.int state 5 in
  List.init count (fun index ->
      List.map
        (fun column ->
          if table.primary_key && column.name = "c1" then Printf.sprintf "(%dL)" (index + 1)
          else value state column)
        (columns table))

let tuple_of values =
  match values with [ value ] -> value | values -> "(" ^ String.concat ", " values ^ ")"

let literal_of column =
  let base =
    match column.kind with
    | Int -> "Check.int64"
    | Uint64 -> "Check.uint64"
    | Double | Decimal -> "Check.float"
    | Text | Datetime -> "Check.text"
    | Blob -> "Check.blob"
    | Bool -> "Check.bool"
    | Json -> "Check.json"
    | Enum values ->
      "(function "
      ^ String.concat " | "
          (List.map (fun (value, variant) -> Printf.sprintf "`%s -> %S" variant ("'" ^ value ^ "'")) values)
      ^ ")"
  in
  if column.nullable then "(Check.nullable " ^ base ^ ")" else base

let runner ~partner state table =
  let columns = columns table in
  let name = table.table_name in
  let rows = rows_of state table in
  let ids = List.init (Random.State.int state 4) (fun _ -> Random.State.int state 2000 - 1000) in
  let cells =
    String.concat "; "
      (List.map (fun column -> Printf.sprintf "%s %s" (literal_of column) column.name) columns)
  in
  let bulk =
    [ Printf.sprintf
        "Check.sql_is (insert_many_%s ~rows)\n\
        \        (Some (%S ^ String.concat \", \" (List.map literal_row rows)))"
        name
        (Printf.sprintf "INSERT INTO %s (%s) VALUES " name (names columns));
      Printf.sprintf "Check.sql_is (insert_many_%s ~rows:[]) None" name ]
  in
  let in_list, lookups =
    if table.primary_key then
      let select = Printf.sprintf "SELECT %s FROM %s WHERE " (names columns) name in
      [ Printf.sprintf "Check.sql_is (lookup_in_%s ~ids:[ %s ]) (Some %S)"
          name
          (String.concat "; " (List.map (Printf.sprintf "(%dL)") ids))
          (match ids with
           | [] -> select ^ "FALSE"
           | ids -> select ^ "c1 IN (" ^ String.concat ", " (List.map string_of_int ids) ^ ")");
        Printf.sprintf "Check.sql_is (lookup_in_%s ~ids:[]) (Some %S)" name (select ^ "FALSE") ],
      Printf.sprintf
        "  Check.lookups\n\
        \    (List.map\n\
        \       (fun row ->\n\
        \         let db = Model_driver.connect () in\n\
        \         ignore (Run.execute db (insert row));\n\
        \         let (%s) = row in\n\
        \         let found = Run.find_opt db (lookup_%s ~c1) in\n\
        \         found = Some row && db.Model_driver.bound = [ Model_driver.Int c1 ])\n\
        \       rows);\n\
        \  Check.lookup_sql (lookup_%s ~c1:0L)\n"
        (names columns) name name
    else [], "  ()\n"
  in
  let literals =
    Printf.sprintf
      "  let literal_row (%s) = \"(\" ^ String.concat \", \" [ %s ] ^ \")\" in\n\
      \  Check.literals\n\
      \    [ %s ];\n"
      (tuple_pattern columns) cells
      (String.concat ";\n      " (bulk @ in_list))
  in
  let sample = scalar state table.columns.first.kind in
  let first_name = table.columns.first.name in
  let iter_seen call =
    Printf.sprintf
      "  let seen = ref [] in\n\
      \  Run.iter db (%s) (fun row -> seen := row :: !seen);\n\
      \  Check.rows ~expected:rows ~actual:(List.rev !seen)"
      call
  in
  let choices =
    "  ;\n"
    ^ String.concat ";\n"
        (List.map iter_seen
           [ Printf.sprintf "pick_%s ~pick:(`By_first %s)" name sample;
             Printf.sprintf "pick_%s ~pick:`Everything" name;
             Printf.sprintf "maybe_%s ~f:(`Some %s)" name sample;
             Printf.sprintf "maybe_%s ~f:`None" name ])
    ^ "\n"
  in
  let nested =
    "  ;\n"
    ^ String.concat ";\n"
        (List.map iter_seen
           [ Printf.sprintf "cte_%s" name;
             Printf.sprintf "cte2_%s" name;
             Printf.sprintf "sub_from_%s" name;
             Printf.sprintf "union_%s" name;
             Printf.sprintf "exists_%s" name;
             Printf.sprintf "in_sub_%s ~%s:%s" name first_name sample;
             Printf.sprintf "pick_sub_%s ~pick:(`By_first %s)" name sample;
             Printf.sprintf "pick_sub_%s ~pick:`Everything" name;
             Printf.sprintf "join_%s ~other:%s" name
               (scalar state partner.columns.first.kind);
             Printf.sprintf "xsub_%s ~other:%s" name
               (scalar state partner.columns.first.kind) ])
    ^ "\n"
  in
  let cte_two_check =
    if table.primary_key then
      Printf.sprintf
        "  ;\n\
        \  let empty = Model_driver.connect () in\n\
        \  let found = Run.find_opt empty (cte_two_%s ~lo:%s ~hi:%s) in\n\
        \  Check.rows ~expected:[] ~actual:(Option.to_list found)\n"
        name sample sample
    else
      Printf.sprintf
        "  ;\n\
        \  let empty = Model_driver.connect () in\n\
        \  let seen = ref [] in\n\
        \  Run.iter empty (cte_two_%s ~lo:%s ~hi:%s) (fun row -> seen := row :: !seen);\n\
        \  Check.rows ~expected:[] ~actual:!seen\n"
        name sample sample
  in
  let mutations =
    let set_v =
      if table.columns.first.nullable then Printf.sprintf "(Some %s)" sample else sample
    in
    Printf.sprintf
      "  ;\n\
      \  Check.holds \"update\" ((Run.execute db (update_%s ~set_v:%s ~where_v:%s)).Model_driver.affected_rows = 1L);\n\
      \  Check.holds \"delete\" ((Run.execute db (delete_pick_%s ~pick:(`By_first %s))).Model_driver.affected_rows = 1L);\n\
      \  Check.holds \"delete\" ((Run.execute db (delete_pick_%s ~pick:`Everything)).Model_driver.affected_rows = 1L)\n"
      name set_v sample name sample name
  in
  Printf.sprintf
    "let () =\n\
    \  let insert (%s) = insert_%s %s in\n\
    \  let rows = [ %s ] in\n\
    \  Check.table ~ddl:%S ~insert:(insert (List.hd rows)) ~select:select_%s;\n\
    \  let db = Model_driver.connect () in\n\
    \  List.iter (fun row -> ignore (Run.execute db (insert row))) rows;\n\
    \  let seen = ref [] in\n\
    \  Run.iter db select_%s (fun row -> seen := row :: !seen);\n\
    \  Check.rows ~expected:rows ~actual:(List.rev !seen);\n\
    \  Check.families ~expected:[ %s ] db;\n\
     %s\n"
    (tuple_pattern columns) name (labelled columns)
    (String.concat "; " (List.map tuple_of rows))
    (ddl table) name name (families columns)
    (literals ^ lookups ^ choices ^ nested ^ cte_two_check ^ mutations)

let () =
  let seed = int_of_string Sys.argv.(1) in
  let prefix = if Array.length Sys.argv > 2 then Sys.argv.(2) else "generated" in
  let generated_module = String.capitalize_ascii prefix in
  let header =
    "module Run = Sqlgg_request.Make (Model_driver)\n\n\
     let failures = ref 0\n\n\
     module Check = struct\n\
    \  type nullability = Required | Nullable\n\n\
    \  let int64 = Int64.to_string\n\
    \  let uint64 = Unsigned.UInt64.to_string\n\
    \  let float = Printf.sprintf \"%h\"\n\
    \  let text = Printf.sprintf \"%S\"\n\
    \  let blob = Printf.sprintf \"X%S\"\n\
    \  let bool = string_of_bool\n\
    \  let json _ = \"JSON\"\n\
    \  let nullable literal = function None -> \"NULL\" | Some value -> literal value\n\n\
    \  let sql request = Option.value (Run.sql request) ~default:\"(nothing to run)\"\n\n\
    \  let table ~ddl ~insert ~select =\n\
    \    Printf.printf \"%s\\n  insert: %s\\n  select: %s\\n\" ddl (sql insert)\n\
    \      (sql select)\n\n\
    \  let verdict label ok count =\n\
    \    if not ok then incr failures;\n\
    \    Printf.printf \"  %s: %s\\n\" label (if ok then Printf.sprintf \"%d ok\" count else \"MISMATCH\")\n\n\
    \  let sql_is request expected = Run.sql request = expected\n\
    \  let literals checks = verdict \"literals\" (List.for_all Fun.id checks) (List.length checks)\n\n\
    \  let rows ~expected ~actual = verdict \"roundtrip\" (expected = actual) (List.length expected)\n\n\
    \  let lookups results = verdict \"lookup\" (List.for_all Fun.id results) (List.length results)\n\n\
    \  let families ~expected db =\n\
    \    let allowed cell (family, nullability) =\n\
    \      Model_driver.family cell = family\n\
    \      || (nullability = Nullable && cell = Model_driver.Null)\n\
    \    in\n\
    \    let rows = db.Model_driver.rows in\n\
    \    verdict \"families\"\n\
    \      (List.for_all (fun row -> List.for_all2 allowed (Array.to_list row) expected) rows)\n\
    \      (List.length rows)\n\n\
    \  let lookup_sql request = Printf.printf \"  lookup: %s\\n\" (sql request)\n\n\
    \  let holds label cond = verdict label cond 1\n\
     end\n\n"
  in
  let footer = "let () = if !failures > 0 then exit 1\n" in
  let state = Random.State.make [| seed |] in
  let tables = List.init 16 (fun index -> random_table state (index + 1)) @ [ kitchen_sink state ] in
  let partner index = List.nth tables ((index + 1) mod List.length tables) in
  write (prefix ^ "_schema.sql") (String.concat "\n" (List.map ddl tables) ^ "\n");
  write (prefix ^ ".ml")
    (String.concat "\n" (List.mapi (fun index table -> implementation ~partner:(partner index) table) tables));
  write (prefix ^ ".mli")
    (String.concat "\n" (List.mapi (fun index table -> interface ~partner:(partner index) table) tables));
  write (prefix ^ "_run.ml")
    (Printf.sprintf "open %s\n\n%s%s%s" generated_module header
       (String.concat "" (List.mapi (fun index table -> runner ~partner:(partner index) state table) tables))
       footer)
