let somes_into_strings =
  List.filter_map (function
    | (key : string), Some (value : string) -> Some (key, `String value)
    | _, None -> None)

let span ?operation ?tables ~system ~sql name thunk =
  let data () =
    let system =
      match system with
      (* From the OpenTelemetry semantic conventions:
         <https://github.com/open-telemetry/semantic-conventions/blob/eff30869/docs/database/database-spans.md#notes-and-well-known-identifiers-for-dbsystem> *)
      | `mariadb -> "mariadb"
      | `mysql -> "mysql"
      | `sqlite -> "sqlite"
    in
    somes_into_strings
      [
        "db.system", Some system;
        "db.statement", Some sql;
        "db.operation", operation;
        "db.table", Option.map List.hd tables;
        "db.tables", Option.map (String.concat ",") tables;
      ]
  in
  Trace_core.with_span ~__FILE__:"" ~__LINE__:0 ~data name @@ fun _ -> thunk ()
