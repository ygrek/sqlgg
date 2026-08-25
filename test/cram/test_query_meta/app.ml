open Printf

let kind_name (kind : Sqlgg_traits.Query.kind) =
  match kind with
  | Select Nat -> "select"
  | Select One -> "select_one"
  | Select Zero_one -> "select_zero_one"
  | Insert table -> "insert " ^ table
  | Update table -> "update " ^ Option.value table ~default:"?"
  | Delete tables -> "delete " ^ String.concat "," tables
  | Create table -> "create " ^ table
  | CreateIndex index -> "create index " ^ index
  | Alter tables -> "alter " ^ String.concat "," tables
  | Drop table -> "drop " ^ table
  | CreateRoutine name -> "create routine " ^ name
  | CreateType name -> "create type " ^ name
  | DropType name -> "drop type " ^ name
  | Other -> "other"

let current_request = ref "req-1"

module Annotating_impl = struct

  include Print_ocaml_impl

  let annotate (q : Sqlgg_traits.Query.t) =
    match q.kind with
    | Create _ | Alter _ | Drop _ -> q
    | _ ->
      Sqlgg_traits.Query.Sqlcommenter.annotate ([
        "app", "users api";
        "query", q.name;
        "kind", kind_name q.kind;
        "request_id", !current_request;
      ] @ (match q.filename with None -> [] | Some file -> [ "file", file ])) q

  let select db q = Print_ocaml_impl.select db (annotate q)
  let select_one db q = Print_ocaml_impl.select_one db (annotate q)
  let select_one_maybe db q = Print_ocaml_impl.select_one_maybe db (annotate q)
  let execute db q = Print_ocaml_impl.execute db (annotate q)

end

module Sql = Output.Sqlgg (Annotating_impl)

let section name = printf "\n=== %s ===\n" name

let () =
  let db = () in

  section "named select, parameters stay bound";
  Print_ocaml_impl.clear_mock_responses ();
  Print_ocaml_impl.setup_select_response [
    Print_ocaml_impl.make_mock_row [ Print_ocaml_impl.mock_int 1L; Print_ocaml_impl.mock_text "alice" ]
  ];
  Sql.find_user ~id:1L db (fun ~id ~name -> printf "row: %Ld %s\n" id name);

  section "unnamed select gets a generated name";
  Print_ocaml_impl.clear_mock_responses ();
  Print_ocaml_impl.setup_select_one_response
    (Some (Print_ocaml_impl.make_mock_row [ Print_ocaml_impl.mock_int 7L ]));
  printf "total: %Ld\n" (Sql.select_2 db);

  section "runtime assembled sql, same name and kind";
  Print_ocaml_impl.clear_mock_responses ();
  Print_ocaml_impl.setup_select_response [];
  Sql.find_users ~ids:[ 1L; 2L; 3L ] db (fun ~id ~name -> printf "row: %Ld %s\n" id name);

  section "batch insert";
  Print_ocaml_impl.clear_mock_responses ();
  Print_ocaml_impl.setup_execute_response ~affected_rows:2L ~insert_id:(Some 10L) ();
  ignore (Sql.add_users ~rows:[ (1L, "alice"); (2L, "bob") ] db);

  section "per request attributes change, sql of the query does not";
  current_request := "req-2";
  Print_ocaml_impl.clear_mock_responses ();
  Print_ocaml_impl.setup_execute_response ~affected_rows:1L ();
  ignore (Sql.rename_user ~name:"carol" ~id:1L db);

  section "ddl is left alone by this implementation";
  Print_ocaml_impl.clear_mock_responses ();
  Print_ocaml_impl.setup_execute_response ~affected_rows:0L ();
  ignore (Sql.create_users db)
