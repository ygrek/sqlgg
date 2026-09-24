open OUnit
open Printf
open Stdlib
open ExtLib
open Sqlgg
open Sql
open Stmt
open Sql_template

let meta = Meta.empty ()

let located ?(value = None) (i1, i2) =
  make_located ~value ~pos:(i1, i2)

let show_list show l = String.concat "; " (List.map show l)

let assert_template ~msg got expected =
  assert_equal ~msg
    ~cmp:(Stdlib.List.equal Sql_template.equal)
    ~printer:(show_list Sql_template.show)
    expected got

let analyze_dml ddl sql =
  let context = Test_helpers.context ddl in
  snd (Test_helpers.get_ok (Test_helpers.analyze ~context sql))

let original _ (bind : bind) = bind.original

let fill_sql ?(spell = original) sql vars = fill ~spell (of_sql sql vars)

let test_lower_static_bind =
  "of_sql `Text `Bind Text" >:: fun () ->
  let sql = "SELECT 1 WHERE id = ?" in
  let p =
    make_param ~id:(located (20, 21)) ~typ:(Type.strict Int)
  in
  assert_template ~msg:"of_sql preserves bind"
    (of_sql sql [ Single (p, meta) ])
    [
      `Text "SELECT 1 WHERE id = ";
      `Bind { param = p; original = "?" };
    ]

let test_lower_bind_in_choice_arm =
  "of_sql bind in Choice arm exact" >:: fun () ->
  let sql = "x( ?)" in
  let choice_id = located ~value:(Some "opt") (1, 5) in
  let bind_param =
    make_param ~id:(located (3, 4)) ~typ:(Type.strict Int)
  in
  let ctor_id = located ~value:(Some "Some") (1, 5) in
  let ctors =
    [
      Sql.Simple
        {
          ctor = ctor_id;
          ctor_pos = (1, 5);
          body = Some [ Single (bind_param, meta) ];
        };
    ]
  in
  assert_template ~msg:"Choice arm keeps `Bind in render"
    (of_sql sql [ Choice (choice_id, ctors) ])
    [
      `Text "x";
      `Choice
        ( choice_id
        , [
            {
              ctor = ctor_id;
              args = Some [ Single (bind_param, meta) ];
              sql =
                [
                  `Text " (";
                  `Text "( ";
                  `Bind { param = bind_param; original = "?" };
                  `Text ")";
                  `Text ") ";
                ];
            };
          ] );
    ]

let test_poly_choice_render_and_squash =
  "poly Choice `Bind squash exact" >:: fun () ->
  let sql = "Q @ch{ a(?) | b }" in
  let ch1 = String.find sql "@ch" in
  let ch2 = ch1 + 3 in
  let a1 = String.find sql "a(?" in
  let a2 = a1 + 4 in
  let b1 = String.find sql " b" + 1 in
  let b2 = b1 + 1 in
  let choice_id = located ~value:(Some "ch") (ch1, ch2) in
  let bind_param =
    make_param ~id:(located (a1 + 2, a1 + 3)) ~typ:(Type.strict Int)
  in
  let ctor_a = located ~value:(Some "A") (a1, a2) in
  let ctor_b = located ~value:(Some "B") (b1, b2) in
  let ctors =
    [
      Sql.Simple
        {
          ctor = ctor_a;
          ctor_pos = (a1, a2);
          body = Some [ Single (bind_param, meta) ];
        };
      Sql.Simple { ctor = ctor_b; ctor_pos = (b1, b2); body = None };
    ]
  in
  let vars = [ Choice (choice_id, ctors) ] in
  assert_template ~msg:"poly Choice of_sql"
    (of_sql sql vars)
    [
      `Text "Q ";
      `Choice
        ( choice_id
        , [
            {
              ctor = ctor_a;
              args = Some [ Single (bind_param, meta) ];
              sql =
                [
                  `Text " (";
                  `Text "a(";
                  `Bind { param = bind_param; original = "?" };
                  `Text ")";
                  `Text ") ";
                ];
            };
            {
              ctor = ctor_b;
              args = None;
              sql = [];
            };
          ] );
      `Text "{ a(?) | b }";
    ];
  assert_template ~msg:"poly Choice subst squash"
    (fill_sql sql vars)
    [
      `Text "Q ";
      `Choice
        ( choice_id
        , [
            {
              ctor = ctor_a;
              args = Some [ Single (bind_param, meta) ];
              sql = [ `Text " (a(?)) " ];
            };
            {
              ctor = ctor_b;
              args = None;
              sql = [];
            };
          ] );
      `Text "{ a(?) | b }";
    ]

let dynamic_select_fixture () =
  let sql = "SELECT @d{ ?, id } FROM t" in
  let d1 = String.find sql "@d" in
  let d2 = d1 + 2 in
  let brace1 = String.find sql "{" in
  let brace2 = String.find sql "}" + 1 in
  let q1 = String.find sql "?" in
  let name = located ~value:(Some "d") (d1, d2) in
  let bind_param =
    make_param ~id:(located (q1, q1 + 1)) ~typ:(Type.strict Int)
  in
  let ctor = located ~value:None (brace1, brace2) in
  let ctors =
    [
      Sql.Simple
        {
          ctor;
          ctor_pos = (brace1, brace2);
          body = Some [ Single (bind_param, meta) ];
        };
    ]
  in
  ( sql
  , name
  , bind_param
  , ctor
  , brace1
  , brace2
  , [ DynamicSelect (name, ctors) ] )

let test_dynamic_select_nested_bind_parity =
  "DynamicSelect nested bind template exact" >:: fun () ->
  let sql, name, bind_param, ctor, _, _, vars = dynamic_select_fixture () in
  assert_template ~msg:"DynamicSelect of_sql"
    (of_sql sql vars)
    [
      `Text "SELECT ";
      `DynamicSelect
        ( name
        , [
            {
              ctor;
              args = Some [ Single (bind_param, meta) ];
              sql =
                [
                  `Text "{ ";
                  `Bind { param = bind_param; original = "?" };
                  `Text ", id }";
                ];
            };
          ] );
      `Text "{ ?, id } FROM t";
    ];
  assert_template ~msg:"DynamicSelect separate bind in arm"
    (fill_sql sql vars)
    [
      `Text "SELECT ";
      `DynamicSelect
        ( name
        , [
            {
              ctor;
              args = Some [ Single (bind_param, meta) ];
              sql = [ `Text "{ "; `Text "?"; `Text ", id }" ];
            };
          ] );
      `Text "{ ?, id } FROM t";
    ]

let test_dynamic_select_subst_callback =
  "DynamicSelect subst order exact" >:: fun () ->
  let sql, name, bind_param, ctor, _, _, vars = dynamic_select_fixture () in
  let subst index _ = sprintf "@b%u" index in
  assert_template ~msg:"DynamicSelect subst in arm only"
    (fill_sql ~spell:subst sql vars)
    [
      `Text "SELECT ";
      `DynamicSelect
        ( name
        , [
            {
              ctor;
              args = Some [ Single (bind_param, meta) ];
              sql = [ `Text "{ "; `Text "@b0"; `Text ", id }" ];
            };
          ] );
      `Text "{ ?, id } FROM t";
    ]

let test_choice_dynamic_in =
  "ChoiceIn `DynamicIn exact" >:: fun () ->
  let sql = "SELECT 1 WHERE id IN (??)" in
  let in_param = located ~value:(Some "ids") (22, 24) in
  let inner =
    make_param ~id:(located (23, 24)) ~typ:(Type.strict Int)
  in
  let vars =
    [
      ChoiceIn
        {
          param = in_param;
          kind = `In;
          vars = [ SingleIn (inner, meta) ];
        };
    ]
  in
  let expected =
    [
      `Text "SELECT 1 WHERE id IN (";
      `DynamicIn (in_param, `In, [ `Text "?"; `SubstIn (inner, meta) ]);
      `Text ")";
    ]
  in
  assert_template ~msg:"ChoiceIn of_sql" (of_sql sql vars) expected;
  assert_template ~msg:"ChoiceIn sql" (fill_sql sql vars) expected

let test_tuple_list_where_in =
  "TupleList Where_in exact" >:: fun () ->
  let sql = "WHERE (a,b) IN ??" in
  let gid = String.find sql "??" in
  let id = located ~value:(Some "pairs") (gid, gid + 2) in
  let kind =
    Where_in
      {
        value = ([ (Type.strict Int, meta); (Type.strict Int, meta) ], `In);
        pos = (gid, gid + 2);
      }
  in
  let vars = [ TupleList (id, kind) ] in
  let expected =
    [
      `Text "WHERE (a,b) IN ";
      `DynamicIn (id, `In, [ `Text ""; `SubstTuple (id, kind) ]);
    ]
  in
  assert_template ~msg:"Where_in of_sql" (of_sql sql vars) expected;
  assert_template ~msg:"Where_in sql" (fill_sql sql vars) expected

let test_option_action_bool_choices_analysis =
  "OptionActionChoice BoolChoices via Analysis" >:: fun () ->
  let ddl = "CREATE TABLE t (id INT NOT NULL, v INT NOT NULL)" in
  let query = "SELECT id FROM t WHERE { id = @v }?" in
  let r = analyze_dml ddl query in
  let stmt = Query.of_result [] r in
  match r.typed.vars with
  | [
   OptionActionChoice
     ( pid
     , [ Single (param, _) ]
     , _
     , BoolChoices );
    ] ->
      let expected bind =
        [
          `Text "SELECT id FROM t WHERE ";
          `Optional
            ( pid
            , {
                vars = [ Single (param, meta) ];
                some = [ `Text " ( "; `Text " id = "; bind; `Text " "; `Text " ) " ];
                none = " TRUE ";
              } );
        ]
      in
      assert_template ~msg:"Query.template"
        (Query.template stmt)
        (expected (`Bind { param; original = "@v" }));
      assert_template ~msg:"filled template None"
        (fill ~spell:original (Query.template stmt))
        (expected (`Text "@v"));
      assert_template ~msg:"filled template subst"
        (fill ~spell:(fun _ _ -> "@bound") (Query.template stmt))
        (expected (`Text "@bound"))
  | _ -> assert_failure "expected OptionActionChoice BoolChoices from Analysis"

let sql_static_text l =
  l
  |> List.filter_map (function `Text s -> Some s | _ -> None)
  |> String.concat ""

let rec option_arm_has_nested_dynamic l =
  match l with
  | [] -> false
  | `Optional (_, ({ some; _ } : _ Sql_template.optional)) :: _ ->
      List.exists
        (function
          | `Choice _ | `Optional _ | `DynamicSelect _ -> true
          | `Text _ | `SubstIn _ | `DynamicIn _ | `SubstTuple _ | `Cond _ -> false)
        some
  | _ :: tl -> option_arm_has_nested_dynamic tl

let test_issue_183_nested_option_action =
  "issue_183 OptionActionChoice nested Choice/IN" >:: fun () ->
  let ddl =
    "CREATE TABLE registration_feedbacks (id INT NOT NULL, user_message TEXT, \
     user_message_2 TEXT)"
  in
  let query =
    "SELECT * FROM registration_feedbacks WHERE\n  id = @id AND\n  { \n   \
     `user_message` = @search \n    OR `user_message` = @search \n    OR \
     `user_message_2` = @search2 \n    OR `user_message_2` IN @xs\n    OR @xss \
     { A { user_message_2 = @a } | B { user_message_2 = @b } }\n}?"
  in
  let r = analyze_dml ddl query in
  let stmt = Query.of_result [] r in
  let sql = fill ~spell:original (Query.template stmt) in
  assert_bool "SQL starts with outer SELECT"
    (Stdlib.String.starts_with ~prefix:"SELECT * FROM registration_feedbacks" (sql_static_text sql));
  assert_bool "nested `Choice inside optional Some arm"
    (option_arm_has_nested_dynamic sql)

let test_fragment_at_body_start =
  "fragments may start where the enclosing body starts" >:: fun () ->
  let ddl = "CREATE TABLE t (id INT NOT NULL, a INT NOT NULL, b INT NOT NULL)" in
  let first_arm query =
    match fill ~spell:original (Query.template (Query.of_result [] (analyze_dml ddl query))) with
    | [ `Text "SELECT id FROM t WHERE "; `Optional (_, ({ some; _ } : _ Sql_template.optional)) ] -> some
    | [ `Text "SELECT id FROM t WHERE "; `Choice (_, ({ sql; _ } : _ Sql_template.arm) :: _) ] -> sql
    | template -> assert_failure (query ^ ": " ^ show_list Sql_template.show template)
  in
  assert_equal ~printer:Fun.id " ( @a = a ) " (sql_static_text (first_arm "SELECT id FROM t WHERE {@a = a}?"));
  assert_equal ~printer:Fun.id " () " (sql_static_text (first_arm "SELECT id FROM t WHERE @c { A {{a = @a}?} | B }"));
  let outer = first_arm "SELECT id FROM t WHERE {{a = @a}? AND b = @b}?" in
  assert_equal ~printer:Fun.id " (  AND b = @b ) " (sql_static_text outer);
  match
    List.filter_map
      (function `Optional (_, ({ some; _ } : _ Sql_template.optional)) -> Some some | _ -> None)
      outer
  with
  | [ inner ] -> assert_equal ~printer:Fun.id " ( a = @a ) " (sql_static_text inner)
  | _ -> assert_failure ("nested fragment: " ^ show_list Sql_template.show outer)

let test_option_bool_syntax_outer_select =
  "option_bool_syntax outer WHERE shape" >:: fun () ->
  let ddl =
    "CREATE TABLE test30 (a INT NOT NULL, b INT NOT NULL); CREATE TABLE test31 \
     (c INT NOT NULL, d INT NOT NULL, r TEXT NOT NULL)"
  in
  let query =
    "SELECT *\nFROM test30\nLEFT JOIN test31 on test31.c = @test31a\nWHERE { c \
     = @choice2 }? OR { r = @choice3 }? OR { c = @choice4 }?\nGROUP BY b"
  in
  let r = analyze_dml ddl query in
  let stmt = Query.of_result [] r in
  let sql = fill ~spell:original (Query.template stmt) in
  assert_bool "starts with SELECT *"
    (Stdlib.String.starts_with ~prefix:"SELECT *" (sql_static_text sql));
  assert_bool "contains GROUP BY tail"
    (List.exists
       (function
         | `Text s -> String.exists s ~sub:"GROUP BY b"
         | _ -> false)
       sql)

let test_option_action_setdefault_analysis =
  "OptionActionChoice SetDefault via Analysis" >:: fun () ->
  let ddl =
    "CREATE TABLE registration_feedbacks (user_message TEXT NOT NULL DEFAULT \
     '', grant_types VARCHAR(80) NOT NULL DEFAULT 'x')"
  in
  let query =
    "INSERT INTO registration_feedbacks SET user_message = { CONCAT(@user_message, \
     '22222') }??"
  in
  let r = analyze_dml ddl query in
  let stmt = Query.of_result [] r in
  match r.typed.vars with
  | [
   OptionActionChoice
     ( pid
     , [ Single (param, _) ]
     , _
     , SetDefault );
    ] ->
      let expected bind =
        [
          `Text "INSERT INTO registration_feedbacks SET user_message = ";
          `Optional
            ( pid
            , {
                vars = [ Single (param, meta) ];
                some = [ `Text " ( "; `Text " CONCAT("; bind; `Text ", '22222') "; `Text " ) " ];
                none = " DEFAULT ";
              } );
        ]
      in
      assert_template ~msg:"Query.template SetDefault"
        (Query.template stmt)
        (expected (`Bind { param; original = "@user_message" }));
      assert_template ~msg:"filled template SetDefault None"
        (fill ~spell:original (Query.template stmt))
        (expected (`Text "@user_message"))
  | _ -> assert_failure "expected OptionActionChoice SetDefault from Analysis"

let test_poly_choice_subst_verbatim =
  "poly Choice bind subst and Verbatim exact" >:: fun () ->
  let sql = " ? AND @ch{ x(?) | picked } AND ?" in
  let q0 = 1 in
  let q1 = String.length sql - 1 in
  let ch1 = String.find sql "@ch" in
  let ch2 = ch1 + 3 in
  let x1 = String.find sql "x(?" in
  let bind_param =
    make_param ~id:(located (x1 + 2, x1 + 3)) ~typ:(Type.strict Int)
  in
  let p0 = make_param ~id:(located (q0, q0 + 1)) ~typ:(Type.strict Int) in
  let p1 = make_param ~id:(located (q1, q1 + 1)) ~typ:(Type.strict Int) in
  let choice_id = located ~value:(Some "ch") (ch1, ch2) in
  let ctor_x = located ~value:(Some "x") (x1, x1 + 4) in
  let ctors =
    [
      Sql.Simple
        {
          ctor = ctor_x;
          ctor_pos = (x1, x1 + 4);
          body = Some [ Single (bind_param, meta) ];
        };
      Verbatim ("picked", "PICKED");
    ]
  in
  let vars =
    [
      Single (p0, meta);
      Choice (choice_id, ctors);
      Single (p1, meta);
    ]
  in
  assert_template ~msg:"poly Choice of_sql bind+verbatim"
    (of_sql sql vars)
    [
      `Text " ";
      `Bind { param = p0; original = "?" };
      `Text " AND ";
      `Choice
        ( choice_id
        , [
            {
              ctor = ctor_x;
              args = Some [ Single (bind_param, meta) ];
              sql =
                [
                  `Text " (";
                  `Text "x(";
                  `Bind { param = bind_param; original = "?" };
                  `Text ")";
                  `Text ") ";
                ];
            };
            {
              ctor = dummy_loc (Some "picked");
              args = Some [];
              sql = [ `Text "PICKED" ];
            };
          ] );
      `Text "{ x(?) | picked } AND ";
      `Bind { param = p1; original = "?" };
    ];
  let subst_top i _ = sprintf "@p%u" i in
  assert_template ~msg:"top-level @p0/@p1 and arm bind index"
    (fill_sql ~spell:subst_top sql vars)
    [
      `Text " @p0 AND ";
      `Choice
        ( choice_id
        , [
            {
              ctor = ctor_x;
              args = Some [ Single (bind_param, meta) ];
              sql = [ `Text " (x(@p0)) " ];
            };
            {
              ctor = dummy_loc (Some "picked");
              args = Some [];
              sql = [ `Text "PICKED" ];
            };
          ] );
      `Text "{ x(?) | picked } AND @p1";
    ]

let test_subst_indices_around_dynamic =
  "subst indices top-level around Choice" >:: fun () ->
  let sql = " ? AND @ch{ a | b } AND ?" in
  let q0 = 1 in
  let q1 = String.length sql - 1 in
  let ch1 = String.find sql "@ch" in
  let ch2 = ch1 + 3 in
  let a1 = String.find sql " a" + 1 in
  let b1 = String.find sql " b" + 1 in
  let p0 = make_param ~id:(located (q0, q0 + 1)) ~typ:(Type.strict Int) in
  let p1 = make_param ~id:(located (q1, q1 + 1)) ~typ:(Type.strict Int) in
  let choice_id = located ~value:(Some "ch") (ch1, ch2) in
  let ctors =
    [
      Sql.Simple
        {
          ctor = located ~value:(Some "a") (a1, a1 + 1);
          ctor_pos = (a1, a1 + 1);
          body = None;
        };
      Sql.Simple
        {
          ctor = located ~value:(Some "b") (b1, b1 + 1);
          ctor_pos = (b1, b1 + 1);
          body = None;
        };
    ]
  in
  let vars =
    [
      Single (p0, meta);
      Choice (choice_id, ctors);
      Single (p1, meta);
    ]
  in
  let subst index _ = sprintf "@p%u" index in
  let ctor_a = located ~value:(Some "a") (a1, a1 + 1) in
  let ctor_b = located ~value:(Some "b") (b1, b1 + 1) in
  assert_template ~msg:"top-level bind indices"
    (fill_sql ~spell:subst sql vars)
    [
      `Text " @p0 AND ";
      `Choice
        ( choice_id
        , [
            { ctor = ctor_a; args = None; sql = [] };
            { ctor = ctor_b; args = None; sql = [] };
          ] );
      `Text "{ a | b } AND @p1";
    ]

let shared_vars_fixture () =
  let shared_sql = "SELECT id FROM t WHERE id = ?" in
  let sq = String.find shared_sql "?" in
  let sp =
    make_param ~id:(located (sq, sq + 1)) ~typ:(Type.strict Int)
  in
  let select_full =
    match Parser.parse_stmt shared_sql with
    | { Parser.stmt = Sql.Select s; _ } -> s
    | _ -> assert_failure "expected SELECT for shared fixture"
  in
  let outer = "SELECT id FROM t WHERE id IN (??)" in
  let gid = String.find outer "??" in
  let group_id = make_located ~value:"sq" ~pos:(gid, gid + 2) in
  let var = SharedVarsGroup ([ Single (sp, meta) ], group_id) in
  (outer, var, sp, group_id, shared_sql, select_full)

let test_shared_vars_group =
  "SharedVarsGroup exact parentheses" >:: fun () ->
  Analysis.with_context (fun () ->
    let outer, var, sp, _group_id, shared_sql, select_full =
      shared_vars_fixture ()
    in
    Shared_queries.add "sq" (shared_sql, select_full);
    assert_template ~msg:"SharedVarsGroup of_sql"
      (of_sql outer [ var ])
      [
        `Text "SELECT id FROM t WHERE id IN ((SELECT id FROM t WHERE id = ";
        `Bind { param = sp; original = "?" };
        `Text "))";
      ];
    assert_template ~msg:"SharedVarsGroup subst squashed"
      (fill_sql outer [ var ])
      [
        `Text
          "SELECT id FROM t WHERE id IN ((SELECT id FROM t WHERE id = ?))";
      ])

let compile_one ~dynamic_select sql =
  match Compile.statement ~dynamic_select (Test_helpers.statement sql) with
  | Executable r -> r
  | Verbatim | Reusable _ | Not_reusable ->
      assert_failure "expected an executable statement"

let test_dynamic_select_plain_arms =
  "DynamicSelect arms without parameters keep their text" >:: fun () ->
  Analysis.with_context @@ fun () ->
  Test_helpers.compile_all "CREATE TABLE users (id INT NOT NULL, name TEXT);\n";
  let r =
    compile_one ~dynamic_select:Props.Only
      "SELECT u.id, u.name FROM users u WHERE u.id = @id"
  in
  let arms =
    List.concat_map
      (function
        | `DynamicSelect (_, arms) -> List.map (fun (arm : _ Sql_template.arm) -> arm.sql) arms
        | _ -> [])
      (fill_sql r.typed.sql r.typed.vars)
  in
  assert_equal
    ~cmp:(Stdlib.List.equal (Stdlib.List.equal Sql_template.equal))
    ~printer:(fun arms -> String.concat " | " (List.map (show_list Sql_template.show) arms))
    [ [ `Text "u.id" ]; [ `Text "u.name" ] ]
    arms

let test_shared_query_with_choice =
  "Choices inside a reused query are sliced from the reused SQL" >:: fun () ->
  Analysis.with_context (fun () ->
    Test_helpers.compile_all
      "CREATE TABLE t (id INT NOT NULL, status INT NOT NULL);\n\
       -- @frag | include: reuse\n\
       SELECT id FROM t WHERE { status = @s }?;\n";
    match compile_one ~dynamic_select:Props.Off "WITH f AS &frag SELECT id FROM f" with
    | { typed = { vars = [ SharedVarsGroup (shared_vars, id) ] as vars; sql; _ }; _ } ->
        let shared_sql, (_ : Sql.select_full) = Shared_queries.get id.value in
        begin match of_sql shared_sql shared_vars with
        | [ `Text before; (`Optional _ as choice) ] ->
            assert_template ~msg:"reused query lowered in place"
              (of_sql sql vars)
              [ `Text ("WITH f AS (" ^ before); choice; `Text ") SELECT id FROM f" ]
        | standalone ->
            assert_failure ("unexpected reused query template: " ^ show_list Sql_template.show standalone)
        end
    | _ -> assert_failure "expected a single shared query reference")

let test_empty_ctor_sentinel =
  "sentinel empty body template and fill exact" >:: fun () ->
  let sql = " a" in
  let choice_id = located ~value:(Some "c") (1, 2) in
  let ctor = located ~value:(Some "None") (1, 0) in
  let ctors =
    [
      Sql.Simple { ctor; ctor_pos = (1, 0); body = None };
    ]
  in
  let vars = [ Choice (choice_id, ctors) ] in
  let expected =
    [
      `Text " ";
      `Choice (choice_id, [ { ctor; args = None; sql = [] } ]);
    ]
  in
  assert_template ~msg:"sentinel of_sql" (of_sql sql vars) expected;
  assert_template ~msg:"sentinel subst" (fill_sql sql vars) expected

let test_invalid_ctor_span =
  "invalid ctor span" >:: fun () ->
  let sql = " a" in
  let choice_id = located ~value:(Some "c") (1, 2) in
  let ctor = located ~value:(Some "Bad") (5, 3) in
  let ctors =
    [
      Sql.Simple
        { ctor; ctor_pos = (5, 3); body = Some [] };
    ]
  in
  try
    ignore (of_sql sql [ Choice (choice_id, ctors) ]);
    assert_failure "expected Invalid_argument for invalid ctor span in Choice"
  with Invalid_argument msg ->
    assert_bool "message mentions stop" (String.exists msg ~sub:"stop")

let test_static_bind =
  "static bind" >:: fun () ->
  let sql = "SELECT 1 WHERE id = ?" in
  let p =
    make_param ~id:(located (20, 21)) ~typ:(Type.strict Int)
  in
  let subst index (bind : bind) =
    sprintf "@p%u(%s)" index (Option.value ~default:"_" bind.param.id.value)
  in
  assert_template ~msg:"named subst"
    (fill_sql ~spell:subst sql [ Single (p, meta) ])
    [ `Text "SELECT 1 WHERE id = @p0(_)" ]

let test_single_in =
  "SingleIn" >:: fun () ->
  let sql = "SELECT 1 WHERE id IN (?)" in
  let p =
    make_param ~id:(located (22, 23)) ~typ:(Type.strict Int)
  in
  assert_template ~msg:"SubstIn"
    (fill_sql sql [ SingleIn (p, meta) ])
    [
      `Text "SELECT 1 WHERE id IN (";
      `SubstIn (p, meta);
      `Text ")";
    ]

let test_subst_tuple =
  "SubstTuple" >:: fun () ->
  let sql = "INSERT INTO t VALUES ??" in
  let id = located ~value:(Some "rows") (21, 23) in
  let kind =
    ValueRows { types = [ Type.strict Int ]; values_start_pos = 21 }
  in
  assert_template ~msg:"tuple list"
    (fill_sql sql [ TupleList (id, kind) ])
    [ `Text "INSERT INTO t VALUES "; `SubstTuple (id, kind) ]

let test_query_name =
  "Query.name" >:: fun () ->
  assert_equal ~msg:"Props.Name"
    "my_query"
    (Query.name [ Props.Name "my_query" ] (Select `Zero_one) 0);
  assert_equal ~msg:"default generated"
    "select_2"
    (Query.name [] (Select `Zero_one) 2)

let test_slice_invalid_stop =
  "Slice invalid stop <= start" >:: fun () ->
  try
    ignore (span_after ~cursor:0 "abc" (2, 1));
    assert_failure "expected Invalid_argument"
  with Invalid_argument msg ->
    assert_bool "message mentions stop" (String.exists msg ~sub:"stop")

let test_slice_invalid_cursor =
  "Slice invalid start < cursor" >:: fun () ->
  try
    ignore (span_after ~cursor:5 "0123456789" (3, 7));
    assert_failure "expected Invalid_argument"
  with Invalid_argument msg ->
    assert_bool "message mentions cursor" (String.exists msg ~sub:"cursor")

let test_overlapping_bind_span =
  "overlapping bind span" >:: fun () ->
  let sql = "??" in
  let p1 =
    make_param ~id:(located (0, 1)) ~typ:(Type.strict Int)
  in
  let p2 =
    make_param ~id:(located (0, 1)) ~typ:(Type.strict Int)
  in
  try
    ignore (of_sql sql [ Single (p1, meta); Single (p2, meta) ]);
    assert_failure "expected Invalid_argument"
  with Invalid_argument _ -> ()

let test_cond_join =
  "Cond join hole" >:: fun () ->
  let sql = "FROM a\nLEFT JOIN b ON b.a = a.id" in
  let j1 = String.find sql "LEFT JOIN b ON b.a = a.id" in
  let j2 = j1 + String.length "LEFT JOIN b ON b.a = a.id" in
  let vars =
    [
      DynamicSelectJoin
        {
          pid = located ~value:(Some "col") (0, 0);
          pos = (j1, j2);
          source =
            {
              table = make_table_name "b";
              alias = None;
            };
        };
    ]
  in
  let join_text = " LEFT JOIN b ON b.a = a.id" in
  assert_template ~msg:"Dep_selected cond"
    (fill_sql sql vars)
    [
      `Text "FROM a";
      `Cond (Dep_selected (located ~value:(Some "col") (0, 0), j1), [ `Text join_text ]);
    ]

let () =
  let suite =
    "query"
    >::: [
           test_lower_static_bind;
           test_lower_bind_in_choice_arm;
           test_poly_choice_render_and_squash;
           test_static_bind;
           test_single_in;
           test_choice_dynamic_in;
           test_tuple_list_where_in;
           test_subst_tuple;
           test_query_name;
           test_dynamic_select_nested_bind_parity;
           test_dynamic_select_subst_callback;
           test_dynamic_select_plain_arms;
           test_option_action_bool_choices_analysis;
           test_option_action_setdefault_analysis;
           test_issue_183_nested_option_action;
           test_fragment_at_body_start;
           test_option_bool_syntax_outer_select;
           test_poly_choice_subst_verbatim;
           test_subst_indices_around_dynamic;
           test_shared_vars_group;
           test_shared_query_with_choice;
           test_slice_invalid_stop;
           test_slice_invalid_cursor;
           test_overlapping_bind_span;
           test_empty_ctor_sentinel;
           test_invalid_ctor_span;
           test_cond_join;
         ]
  in
  let results = run_test_tt suite in
  exit
  @@ if List.exists (function RFailure _ | RError _ -> true | _ -> false) results
  then 1
  else 0
