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

Implement set default feature:

  $ cat set_default_syntax.sql | sqlgg -params unnamed -gen caml - > output.ml

  $ sed -n '17,38p' output.ml
    let insert_registration_feedbacks_1 db ~user_message ~grant_types =
      let set_params stmt =
        let p = T.start_params stmt (0 + (match user_message with Some _ -> 1 | None -> 0) + (match grant_types with Some (grant_types) -> 0 + (match grant_types with `A -> 0 | `B -> 0) | None -> 0)) in
        begin match user_message with
        | None -> ()
        | Some (user_message) ->
          T.set_param_Text p user_message;
        end;
        begin match grant_types with
        | None -> ()
        | Some (grant_types) ->
          begin match grant_types with
          | `A -> ()
          | `B -> ()
          end;
        end;
        T.finish_params p
      in
      T.execute db ("INSERT INTO `registration_feedbacks`\n\
  SET\n\
    `user_message` = " ^ (match user_message with Some _ -> " ( " ^ " CONCAT(" ^ "?" ^ ", '22222') " ^ " ) " | None -> " DEFAULT ") ^ ",\n\
    `grant_types` = " ^ (match grant_types with Some (grant_types) -> " ( " ^ " " ^ (match grant_types with `A -> "'2'" | `B -> "'2'") ^ " " ^ " ) " | None -> " DEFAULT ")) set_params
