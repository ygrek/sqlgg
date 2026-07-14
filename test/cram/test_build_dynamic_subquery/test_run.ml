(* test_run.ml - exercise dynamic select over subquery sources.

   For each scenario we pick different runtime columns and let the printing mock
   traits implementation echo the final SQL ([SQL] ...), so the test shows exactly
   how the chosen columns are spliced into the (sub)query. *)

open Printf

module M (T: Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string and
  type Types.Decimal.t = float and
  type Types.Any.t = string) = struct

  module Sql = Output.Sqlgg(T)
  open Sql

  let prep () =
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_response []

  (* Group 1: pass-through SELECT * over a subquery -> pushdown into the subquery *)
  module Test1 = struct
    open Sql.Star_over_subq

    let run c =
      printf "[1.1] pick id\n";    prep (); ignore (List.select c id ~min:10L (fun x -> x));
      printf "[1.2] pick name\n";  prep (); ignore (List.select c name ~min:10L (fun x -> x));
      printf "[1.3] pick price\n"; prep (); ignore (List.select c price ~min:10L (fun x -> x));
      printf "[1.4] pick id + name + price\n"; prep ();
        ignore (List.select c (let+ i = id and+ n = name and+ p = price in (i, n, p)) ~min:10L (fun x -> x))
  end

  (* Group 2: pass-through via explicit sub.* *)
  module Test2 = struct
    open Sql.Star_alias_over_subq

    let run c =
      printf "[2.1] pick name\n";        prep (); ignore (List.select c name (fun x -> x));
      printf "[2.2] pick id + price\n";  prep ();
        ignore (List.select c (let+ i = id and+ p = price in (i, p)) (fun x -> x))
  end

  (* Group 3: pass-through over a LEFT JOIN subquery (nullable joined columns) *)
  module Test3 = struct
    open Sql.Star_over_join_subq

    let run c =
      printf "[3.1] pick uid\n";                 prep (); ignore (List.select c uid (fun x -> x));
      printf "[3.2] pick ototal (nullable)\n";   prep (); ignore (List.select c ototal (fun x -> x));
      printf "[3.3] pick uid + uname + ototal\n"; prep ();
        ignore (List.select c (let+ a = uid and+ b = uname and+ d = ototal in (a, b, d)) (fun x -> x))
  end

  (* Group 4: non-pass-through outer -> dynamic stays outside, subquery fixed *)
  module Test4 = struct
    open Sql.Cols_over_subq

    let run c =
      printf "[4.1] pick id\n";          prep (); ignore (List.select c id ~id:1L (fun x -> x));
      printf "[4.2] pick id + name\n";   prep ();
        ignore (List.select c (let+ i = id and+ n = name in (i, n)) ~id:1L (fun x -> x))
  end

  let run_all c =
    printf "--- Group 1: SELECT * over subquery (pushdown) ---\n"; Test1.run c;
    printf "--- Group 2: SELECT sub.* over subquery (pushdown) ---\n"; Test2.run c;
    printf "--- Group 3: SELECT * over LEFT JOIN subquery ---\n"; Test3.run c;
    printf "--- Group 4: non-pass-through (dynamic stays outside) ---\n"; Test4.run c
end

module Test = M(Print_ocaml_impl)

let () =
  let con = () in
  printf "Dynamic Select over Subquery Sources\n";
  printf "%s\n" (String.make 50 '=');
  Test.run_all con
