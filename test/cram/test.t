Issue183 is fixed:
  $ cat registration_feedbacks.sql | sqlgg -gen caml - > output.ml

  $ grep -n "let p = T.start_params stmt" output.ml | head -1
  25:      let p = T.start_params stmt (1 + (match search with Some (_, _, xs, xss) -> 3 + (match xs with [] -> 0 | _ :: _ -> 0) + (match xss with `A _ -> 1 | `B _ -> 1) | None -> 0)) in

  $ sed -n '27,44p' output.ml
        begin match search with
        | None -> ()
        | Some (search,search2,xs,xss) ->
          T.set_param_Text p search;
          T.set_param_Text p search;
          T.set_param_Text p search2;
          begin match xs with
          | [] -> ()
          | _ :: _ ->
            ()
          end;
          begin match xss with
          | `A (a) ->
            T.set_param_Text p a;
          | `B (b) ->
            T.set_param_Text p b;
          end;
        end;

  $ sed -n '49p' output.ml | grep -o "match search with Some (_, _, xs, xss)"
  match search with Some (_, _, xs, xss)

Support reusable queries as CTE:
  $ cat reusabe_queries_as_ctes.sql | sqlgg -gen caml - > output.ml

Compare test2 (with &abcd substitution) and test3 (without substitution):
  $ sed -n '125,139p' output.ml > test2_part.tmp
  $ sed -n '156,170p' output.ml > test3_part.tmp
  $ diff test2_part.tmp test3_part.tmp
  $ echo $?
  0

Compare SQL queries in test2 (with &abcd substitution) and test3 (without substitution) # 2:
  $ sed -n '140,150p' output.ml > test2_sql.tmp
  $ sed -n '171,181p' output.ml > test3_sql.tmp
  $ diff test2_sql.tmp test3_sql.tmp
  $ echo $?
  0
