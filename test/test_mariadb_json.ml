open OUnit2

module Mock_blocking = struct
  include Mariadb.Blocking
  type 'a future = 'a
  
  module Field = struct
    type value = Mariadb.Blocking.Field.value
    type row = unit
    
    type t = {
      field_name: string;
      field_value: value;
      field_is_null: bool;
    }
    
    let name f = f.field_name
    let value f = f.field_value
    let null_value f = f.field_is_null
    let can_be_null _f = true
    let time _f = failwith "time not implemented in mock"
    
    let int _f = failwith "not implemented currently"
    let int64 _f = failwith "not implemented currently"
    let uint64 _f = failwith "not implemented currently"
    let float _f = failwith "not implemented currently"
    let string _f = failwith "not implemented currently"
    let bytes _f = failwith "not implemented currently"
    
    let int_opt _f = None
    let int64_opt _f = None
    let uint64_opt _f = None
    let float_opt _f = None
    let string_opt _f = None
    let bytes_opt _f = None
    let time_opt _f = None
    
    let json _f = failwith "not implemented currently"
    let json_opt _f = None
  end
  
  module Row = struct
    module type S = sig
      type t
      val build : int -> (int -> Field.t) -> t
    end
    
    module Array : S with type t = Field.t array = struct
      type t = Field.t array
      let build n f = Array.init n f
    end
    
    module StringMap = Map.Make(String)
    module Map : S with type t = Field.t StringMap.t = struct
      type t = Field.t StringMap.t
      let build _n _f = StringMap.empty
    end
    
    module Hashtbl : S with type t = (string, Field.t) Hashtbl.t = struct
      type t = (string, Field.t) Hashtbl.t
      let build _n _f = Hashtbl.create 0
    end
  end
  
  module Res = struct
    include Mariadb.Blocking.Res
    let fetch (type a) (module _ : Row.S with type t = a) _r = 
      Ok None 
  end
end

module Types = Sqlgg_mariadb.Default_types(Mock_blocking)

let mock_string_field name str =
  { Mock_blocking.Field.field_name = name; field_value = `String str; field_is_null = false }

let mock_bytes_field name str =
  { Mock_blocking.Field.field_name = name; field_value = `Bytes (Bytes.of_string str); field_is_null = false }

let mock_json_field name json_str =
  { Mock_blocking.Field.field_name = name; field_value = `Json json_str; field_is_null = false }

let test_json_from_string _ctx =
  let field = mock_string_field "provider_specific_metrics" "{\"c\":0,\"rs\":0}" in
  let result = Types.Json.of_field field in
  let result_str = Yojson.Safe.to_string (result :> Yojson.Safe.t) in
  assert_equal ~printer:(fun x -> x) "{\"c\":0,\"rs\":0}" result_str

let test_json_from_bytes _ctx =
  let field = mock_bytes_field "provider_specific_metrics" "{\"c\":10,\"rs\":5}" in
  let result = Types.Json.of_field field in
  let result_str = Yojson.Safe.to_string (result :> Yojson.Safe.t) in
  assert_equal ~printer:(fun x -> x) "{\"c\":10,\"rs\":5}" result_str

let test_json_from_json_field _ctx =
  let field = mock_json_field "provider_specific_metrics" "{\"c\":15,\"rs\":3}" in
  let result = Types.Json.of_field field in
  let result_str = Yojson.Safe.to_string (result :> Yojson.Safe.t) in
  assert_equal ~printer:(fun x -> x) "{\"c\":15,\"rs\":3}" result_str

let suite =
  "MariaDB JSON Field Type Handling" >::: [
    "Case 1: JSON as String (old MariaDB)" >:: test_json_from_string;
    "Case 2: JSON as Bytes" >:: test_json_from_bytes;
    "Case 3: JSON as Json type (NEW - bug case)" >:: test_json_from_json_field;
  ]

let () =
  run_test_tt_main suite

