(* Test for MariaDB JSON field type handling *)

(* This test demonstrates the bug where MariaDB returns JSON columns
   with the `Json field type (not `String or `Bytes), which causes
   sqlgg_mariadb.ml to fail with convfail error.
   
   The bug scenario:
   - MariaDB stores JSON in a JSON column type
   - When querying, MariaDB returns the field with type `Json 
   - The old handle_with_json function only handled `String and `Bytes
   - This caused: convfail "expected json provider_specific_metrics, but found json ..."
*)

module Test_Json_Field_Types = struct
  
  (* Simulate MariaDB field value type *)
  type field_value = [
    | `Null
    | `Int of int
    | `Float of float  
    | `String of string
    | `Bytes of bytes
    | `Decimal of string
    | `Json of string    (* This is the case that was missing! *)
    | `Time of int
  ]
  
  type field = {
    name: string;
    value: field_value;
  }
  
  (* Simulate the handle_with_json function BEFORE the fix *)
  let handle_with_json_broken field = function
    | `String x -> Ok (Yojson.Safe.from_string x)
    | `Bytes x -> Ok (Yojson.Safe.from_string (Bytes.to_string x))
    | #field_value as value ->
        (* This catches `Json and others, causing the bug *)
        Error (Printf.sprintf "expected json %s, but found %s" 
                 field.name
                 (match value with
                  | `Json x -> Printf.sprintf "json %S" x
                  | `Int x -> Printf.sprintf "int %d" x
                  | `Float x -> Printf.sprintf "float %f" x
                  | `Null -> "null"
                  | `Decimal x -> Printf.sprintf "decimal %S" x
                  | `Time _ -> "time"
                  | _ -> "unknown"))
  
  (* Simulate the handle_with_json function AFTER the fix *)
  let handle_with_json_fixed field = function
    | `String x -> Ok (Yojson.Safe.from_string x)
    | `Bytes x -> Ok (Yojson.Safe.from_string (Bytes.to_string x))
    | `Json x -> Ok (Yojson.Safe.from_string x)  (* THE FIX! *)
    | #field_value as value ->
        Error (Printf.sprintf "expected json %s, but found %s"
                 field.name
                 (match value with
                  | `Int x -> Printf.sprintf "int %d" x
                  | `Float x -> Printf.sprintf "float %f" x
                  | `Null -> "null"
                  | `Decimal x -> Printf.sprintf "decimal %S" x
                  | `Time _ -> "time"
                  | _ -> "unknown"))
  
  let test_cases = [
    (* Case 1: JSON returned as String (old MariaDB behavior) *)
    { name = "provider_specific_metrics";
      value = `String "{\"c\": 0, \"rs\": 0}" };
    
    (* Case 2: JSON returned as Bytes *)
    { name = "provider_specific_metrics";
      value = `Bytes (Bytes.of_string "{\"c\": 10, \"rs\": 5}") };
    
    (* Case 3: JSON returned as Json type (NEW MariaDB behavior) - THE BUG CASE! *)
    { name = "provider_specific_metrics"; 
      value = `Json "{\"c\": 15, \"rs\": 3}" };
  ]
  
  let run_tests () =
    Printf.printf "\n=== Testing MariaDB JSON Field Type Handling ===\n\n";
    
    List.iteri (fun i field ->
      Printf.printf "Test Case %d: field=%s, type=%s\n" 
        (i+1)
        field.name
        (match field.value with
         | `String _ -> "String"
         | `Bytes _ -> "Bytes"
         | `Json _ -> "Json"
         | _ -> "Other");
      
      (* Test with broken version *)
      Printf.printf "  BROKEN version: ";
      (match handle_with_json_broken field field.value with
       | Ok json -> 
           Printf.printf "SUCCESS - parsed: %s\n" 
             (Yojson.Safe.to_string json)
       | Error msg ->
           Printf.printf "FAILED - %s\n" msg);
      
      (* Test with fixed version *)
      Printf.printf "  FIXED version:  ";
      (match handle_with_json_fixed field field.value with
       | Ok json ->
           Printf.printf "SUCCESS - parsed: %s\n"
             (Yojson.Safe.to_string json)
       | Error msg ->
           Printf.printf "FAILED - %s\n" msg);
      
      Printf.printf "\n"
    ) test_cases;
    
    Printf.printf "=== Summary ===\n";
    Printf.printf "The broken version fails on Case 3 (Json field type)\n";
    Printf.printf "The fixed version handles all three cases correctly\n\n"
end

let () = Test_Json_Field_Types.run_tests ()

