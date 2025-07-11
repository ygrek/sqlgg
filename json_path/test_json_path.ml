open Printf
open OUnit
open Sqlgg_json_path
open Ast

let cmp_path_leg l1 l2 = match l1, l2 with
  | Member s1, Member s2 -> s1 = s2
  | ArrayAccess idx1, ArrayAccess idx2 -> idx1 = idx2
  | ArrayRange r1, ArrayRange r2 -> r1.start_idx = r2.start_idx && r1.end_idx = r2.end_idx
  | MemberWildcard, MemberWildcard | DoubleWildcard, DoubleWildcard -> true
  | _ -> false

let cmp_json_path p1 p2 = 
  p1.scope = p2.scope && 
  (try List.for_all2 cmp_path_leg p1.legs p2.legs with _ -> false)

let parse_path input =
  try
    Json_path.parse_json_path input
  with
  | Failure msg -> assert_failure @@ sprintf "failed to parse: %s : %s" msg input

let test_path input expected_legs =
  let expected = { scope = "$"; legs = expected_legs } in
  let parsed = parse_path input in
  assert_equal 
    ~msg:("path: " ^ input)
    ~cmp:cmp_json_path 
    ~printer:Json_path.string_of_json_path
    expected 
    parsed

let tt input expected_legs =
  let test () = test_path input expected_legs in
  input >:: test

let wrong input =
  input >:: (fun () -> 
    ("Expected error in: " ^ input) @? 
    (try ignore (parse_path input); false with _ -> true))

let test_basic = [
  tt "$" [];
  tt "$.name" [Member "name"];
  tt "$[0]" [ArrayAccess (Index 0)];
  tt "$[*]" [ArrayAccess Wildcard];
  tt "$[last]" [ArrayAccess Last];
  tt "$[last-1]" [ArrayAccess (LastOffset 1)];
  tt "$.*" [MemberWildcard];
  tt "$**" [DoubleWildcard];
]

let test_complex = [
  tt "$.user.name" [Member "user"; Member "name"];
  tt "$.items[0].price" [Member "items"; ArrayAccess (Index 0); Member "price"];
  tt "$.users[*].email" [Member "users"; ArrayAccess Wildcard; Member "email"];
  tt "$[1 to 3]" [ArrayRange { start_idx = Index 1; end_idx = Index 3 }];
  tt "$.\"quoted key\"" [Member "quoted key"];
  tt "$**.name" [DoubleWildcard; Member "name"];
]

let test_ranges = [
  tt "$[0 to 5]" [ArrayRange { start_idx = Index 0; end_idx = Index 5 }];
  tt "$[last-3 to last]" [ArrayRange { start_idx = LastOffset 3; end_idx = Last }];
  tt "$[* to last]" [ArrayRange { start_idx = Wildcard; end_idx = Last }];
]

let test_errors = [
  wrong "";
  wrong "name";
  wrong "$.";
  wrong "$[";
  wrong "$]";
  wrong "$[abc]";
  wrong "$.123invalid";
  wrong "$[1 to]";
  wrong "$[to 5]";
]

let test_combinators = [
  "root" >:: (fun () ->
    let open Syntax in
    let from_combinator = root in
    let from_parser = parse_path "$" in
    let expected = { scope = "$"; legs = [] } in
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path expected from_combinator;
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path from_combinator from_parser
  );
  
  "simple member" >:: (fun () ->
    let open Syntax in
    let from_combinator = root / ~."name" in
    let from_parser = parse_path "$.name" in
    let expected = { scope = "$"; legs = [Member "name"] } in
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path expected from_combinator;
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path from_combinator from_parser
  );
  
  "chained" >:: (fun () ->
    let open Syntax in
    let from_combinator = root / ~."user" / ~."email" in
    let from_parser = parse_path "$.user.email" in
    let expected = { scope = "$"; legs = [Member "user"; Member "email"] } in
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path expected from_combinator;
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path from_combinator from_parser
  );
  
  "with functions" >:: (fun () ->
    let open Syntax in
    let from_combinator = root / ~."items" / any / ~."price" in
    let from_parser = parse_path "$.items[*].price" in
    let expected = { scope = "$"; legs = [Member "items"; ArrayAccess Wildcard; Member "price"] } in
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path expected from_combinator;
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path from_combinator from_parser
  );
  
  "array index" >:: (fun () ->
    let open Syntax in
    let from_combinator = root / idx 5 in
    let from_parser = parse_path "$[5]" in
    let expected = { scope = "$"; legs = [ArrayAccess (Index 5)] } in
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path expected from_combinator;
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path from_combinator from_parser
  );
  
  "last element" >:: (fun () ->
    let open Syntax in
    let from_combinator = root / last in
    let from_parser = parse_path "$[last]" in
    let expected = { scope = "$"; legs = [ArrayAccess Last] } in
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path expected from_combinator;
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path from_combinator from_parser
  );
  
  "member wildcard" >:: (fun () ->
    let open Syntax in
    let from_combinator = root / wildcard in
    let from_parser = parse_path "$.*" in
    let expected = { scope = "$"; legs = [MemberWildcard] } in
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path expected from_combinator;
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path from_combinator from_parser
  );
  
  "range" >:: (fun () ->
    let open Syntax in
    let from_combinator = root / range 1 5 in
    let from_parser = parse_path "$[1 to 5]" in
    let expected = { scope = "$"; legs = [ArrayRange { start_idx = Index 1; end_idx = Index 5 }] } in
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path expected from_combinator;
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path from_combinator from_parser
  );
  
  "complex chain" >:: (fun () ->
    let open Syntax in
    let from_combinator = root / ~."data" / any / ~."users" / last / ~."settings" in
    let from_parser = parse_path "$.data[*].users[last].settings" in
    let expected = { scope = "$"; legs = [
      Member "data";
      ArrayAccess Wildcard;
      Member "users";
      ArrayAccess Last;
      Member "settings"
    ] } in
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path expected from_combinator;
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path from_combinator from_parser
  );
  
  "multiple legs operator" >:: (fun () ->
    let open Syntax in
    let from_combinator = root // [~."api"; ~."v1"; idx 0; ~."data"] in
    let from_parser = parse_path "$.api.v1[0].data" in
    let expected = { scope = "$"; legs = [
      Member "api";
      Member "v1";
      ArrayAccess (Index 0);
      Member "data"
    ] } in
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path expected from_combinator;
    assert_equal ~cmp:cmp_json_path ~printer:Json_path.string_of_json_path from_combinator from_parser
  );
]
let test_round_trip = [
  "round trip basic" >:: (fun () ->
    let inputs = ["$"; "$.name"; "$[0]"; "$.user.email"; "$[1 to 3]"] in
    List.iter (fun input ->
      let parsed = parse_path input in
      let serialized = Json_path.string_of_json_path parsed in
      assert_equal ~msg:("round trip: " ^ input) input serialized
    ) inputs
  );
]

let test_serialization = [
  "serialize basic paths" >:: (fun () ->
    let test_cases = [
      ({ scope = "$"; legs = [] }, "$");
      ({ scope = "$"; legs = [Member "name"] }, "$.name");
      ({ scope = "$"; legs = [ArrayAccess (Index 0)] }, "$[0]");
      ({ scope = "$"; legs = [ArrayAccess Wildcard] }, "$[*]");
      ({ scope = "$"; legs = [ArrayAccess Last] }, "$[last]");
      ({ scope = "$"; legs = [ArrayAccess (LastOffset 1)] }, "$[last-1]");
      ({ scope = "$"; legs = [MemberWildcard] }, "$.*");
      ({ scope = "$"; legs = [DoubleWildcard] }, "$**");
    ] in
    List.iter (fun (path, expected) ->
      let result = Json_path.string_of_json_path path in
      assert_equal ~msg:("serialize: " ^ expected) expected result
    ) test_cases
  );
  
  "serialize complex paths" >:: (fun () ->
    let test_cases = [
      ({ scope = "$"; legs = [Member "user"; Member "name"] }, "$.user.name");
      ({ scope = "$"; legs = [Member "items"; ArrayAccess (Index 0); Member "price"] }, "$.items[0].price");
      ({ scope = "$"; legs = [ArrayRange { start_idx = Index 1; end_idx = Index 3 }] }, "$[1 to 3]");
      ({ scope = "$"; legs = [ArrayRange { start_idx = LastOffset 3; end_idx = Last }] }, "$[last-3 to last]");
      ({ scope = "$"; legs = [DoubleWildcard; Member "name"] }, "$**.name");
    ] in
    List.iter (fun (path, expected) ->
      let result = Json_path.string_of_json_path path in
      assert_equal ~msg:("serialize: " ^ expected) expected result
    ) test_cases
  );
  
  "serialize edge cases" >:: (fun () ->
    let test_cases = [
      ({ scope = "$"; legs = [Member ""] }, "$.");
      ({ scope = "$"; legs = [ArrayAccess (Index 999)] }, "$[999]");
      ({ scope = "$"; legs = [ArrayAccess (LastOffset 100)] }, "$[last-100]");
      ({ scope = "$"; legs = [ArrayRange { start_idx = Wildcard; end_idx = Last }] }, "$[* to last]");
    ] in
    List.iter (fun (path, expected) ->
      let result = Json_path.string_of_json_path path in
      assert_equal ~msg:("serialize: " ^ expected) expected result
    ) test_cases
  );
]

let test_bidirectional = [
  "parse then serialize" >:: (fun () ->
    let inputs = [
      "$"; "$.name"; "$[0]"; "$[*]"; "$[last]"; "$[last-1]"; "$.*"; "$**";
      "$.user.name"; "$.items[0].price"; "$.users[*].email"; 
      "$[1 to 3]"; "$[0 to 5]"; "$[last-3 to last]"; "$[* to last]";
      "$**.name"; "$.\"quoted key\"";
    ] in
    List.iter (fun input ->
      let parsed = parse_path input in
      let serialized = Json_path.string_of_json_path parsed in
      assert_equal ~msg:("bidirectional: " ^ input) input serialized
    ) inputs
  );
  
  "serialize then parse" >:: (fun () ->
    let paths = [
      { scope = "$"; legs = [] };
      { scope = "$"; legs = [Member "test"] };
      { scope = "$"; legs = [ArrayAccess (Index 42)] };
      { scope = "$"; legs = [Member "data"; ArrayAccess Wildcard; Member "value"] };
      { scope = "$"; legs = [ArrayRange { start_idx = Index 5; end_idx = Index 10 }] };
    ] in
    List.iter (fun original_path ->
      let serialized = Json_path.string_of_json_path original_path in
      let parsed_back = parse_path serialized in
      assert_equal 
        ~cmp:cmp_json_path 
        ~printer:Json_path.string_of_json_path
        ~msg:("serialize->parse: " ^ serialized)
        original_path 
        parsed_back
    ) paths
  );
]

let test_validation = [
  "is_valid positive cases" >:: (fun () ->
    let valid_inputs = [
      "$"; "$.name"; "$[0]"; "$[*]"; "$[last]"; "$[last-1]"; "$.*"; "$**";
      "$.user.name"; "$.items[0].price"; "$[1 to 3]"; "$**.field";
    ] in
    List.iter (fun input ->
      assert_bool ("should be valid: " ^ input) (Json_path.is_valid input)
    ) valid_inputs
  );
  
  "is_valid negative cases" >:: (fun () ->
    let invalid_inputs = [
      ""; "name"; "$."; "$["; "$]"; "$[abc]"; "$.123invalid"; 
      "$[1 to]"; "$[to 5]"; "$[1 to 3 to 5]"; "$$"; "$..";
    ] in
    List.iter (fun input ->
      assert_bool ("should be invalid: " ^ input) (not (Json_path.is_valid input))
    ) invalid_inputs
  );
]

let run () =
  let tests = [
    "basic" >::: test_basic;
    "complex" >::: test_complex;
    "ranges" >::: test_ranges;
    "errors" >::: test_errors;
    "combinators" >::: test_combinators;
    "serialization" >::: test_serialization;
    "bidirectional" >::: test_bidirectional;
    "validation" >::: test_validation;
    "round_trip" >::: test_round_trip;
  ] in
  let test_suite = "JSON Path Parser" >::: tests in
  let results = run_test_tt test_suite in
  exit @@ if List.exists (function RFailure _ | RError _ -> true | _ -> false) results then 1 else 0

let _ = run ()
