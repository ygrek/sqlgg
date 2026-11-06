type zero_or_one = Zero_or_one
type exactly_one = Exactly_one
type many = Many
type affected_rows = Affected_rows

type _ scalar =
  | Bool : bool scalar
  | Int : int64 scalar
  | UInt64 : Unsigned.UInt64.t scalar
  | Float : float scalar
  | Decimal : float scalar
  | Text : string scalar
  | Blob : string scalar
  | Datetime : string scalar
  | Json : Sqlgg_trait_types.json scalar
  | Json_path : Sqlgg_trait_types.json_path scalar
  | One_or_all : Sqlgg_trait_types.one_or_all scalar
  | Enum : (module Sqlgg_traits.Enum with type t = 'a) -> 'a scalar

type _ ty =
  | Required : 'a scalar -> 'a ty
  | Optional : 'a scalar -> 'a option ty

type binder = { bind : 'a. 'a ty -> 'a -> unit }

type literals = { literal : 'a. 'a ty -> 'a -> string }

type _ columns =
  | Build : 'build -> 'build columns
  | Column : ('x -> 'build) columns * 'x ty -> 'build columns

type _ outcome =
  | Sql : string -> 'cardinality outcome
  | Nothing_to_run : affected_rows outcome

type ('output, 'cardinality) t = {
  sql : literals -> 'cardinality outcome;
  name : string;
  kind : Sqlgg_query.kind;
  params : binder -> unit;
  result : 'output columns;
}

module Make (T : Sqlgg_traits.M_io) = struct
  let bind_scalar : type a. a scalar -> T.params -> a -> unit =
   fun scalar params value ->
    match scalar with
    | Bool -> T.set_param_bool params value
    | Int -> T.set_param_int64 params value
    | UInt64 -> T.set_param_uint64 params value
    | Float -> T.set_param_float params value
    | Decimal -> T.set_param_decimal params value
    | Text -> T.set_param_string params value
    | Blob -> T.set_param_string params value
    | Datetime -> T.set_param_string params value
    | Json -> T.set_param_json params value
    | Json_path -> T.set_param_json_path params value
    | One_or_all -> T.set_param_one_or_all params value
    | Enum (module E) ->
      let module Codec = T.Make_enum (E) in
      Codec.set_param params value

  let scalar_literal : type a. a scalar -> a -> string =
   fun scalar value ->
    match scalar with
    | Bool -> T.Types.Bool.bool_to_literal value
    | Int -> T.Types.Int.int64_to_literal value
    | UInt64 -> T.Types.UInt64.uint64_to_literal value
    | Float -> T.Types.Float.float_to_literal value
    | Decimal -> T.Types.Decimal.float_to_literal value
    | Text -> T.Types.Text.string_to_literal value
    | Blob -> T.Types.Blob.string_to_literal value
    | Datetime -> T.Types.Text.string_to_literal value
    | Json -> T.Types.Json.json_to_literal value
    | Json_path -> T.Types.Json_path.json_path_to_literal value
    | One_or_all -> T.Types.One_or_all.to_literal value
    | Enum (module E) ->
      let module Codec = T.Make_enum (E) in
      Codec.to_literal value

  let literal : type a. a ty -> a -> string =
   fun ty value ->
    match ty, value with
    | Required scalar, value -> scalar_literal scalar value
    | Optional _, None -> "NULL"
    | Optional scalar, Some value -> scalar_literal scalar value

  let literals = { literal }

  let get_scalar : type a. a scalar -> T.row -> int -> a =
   fun scalar row index ->
    match scalar with
    | Bool -> T.get_column_bool row index
    | Int -> T.get_column_int64 row index
    | UInt64 -> T.get_column_uint64 row index
    | Float -> T.get_column_float row index
    | Decimal -> T.get_column_decimal row index
    | Text -> T.get_column_string row index
    | Blob -> T.get_column_string row index
    | Datetime -> T.get_column_datetime row index
    | Json -> T.get_column_json row index
    | Json_path -> T.get_column_json_path row index
    | One_or_all -> T.get_column_one_or_all row index
    | Enum (module E) ->
      let module Codec = T.Make_enum (E) in
      Codec.get_column row index

  let get_scalar_nullable : type a. a scalar -> T.row -> int -> a option =
   fun scalar row index ->
    match scalar with
    | Bool -> T.get_column_bool_nullable row index
    | Int -> T.get_column_int64_nullable row index
    | UInt64 -> T.get_column_uint64_nullable row index
    | Float -> T.get_column_float_nullable row index
    | Decimal -> T.get_column_decimal_nullable row index
    | Text -> T.get_column_string_nullable row index
    | Blob -> T.get_column_string_nullable row index
    | Datetime -> T.get_column_datetime_nullable row index
    | Json -> T.get_column_json_nullable row index
    | Json_path -> T.get_column_json_path_nullable row index
    | One_or_all -> T.get_column_one_or_all_nullable row index
    | Enum (module E) ->
      let module Codec = T.Make_enum (E) in
      Codec.get_column_nullable row index

  let bind : type a. a ty -> T.params -> a -> unit =
   fun ty params value ->
    match ty, value with
    | Required scalar, value -> bind_scalar scalar params value
    | Optional _, None -> T.set_param_null params
    | Optional scalar, Some value -> bind_scalar scalar params value

  let get : type a. a ty -> T.row -> int -> a =
   fun ty row index ->
    match ty with
    | Required scalar -> get_scalar scalar row index
    | Optional scalar -> get_scalar_nullable scalar row index

  let rec get_columns : type build. build columns -> T.row -> build * int =
   fun columns row ->
    match columns with
    | Build build -> build, 0
    | Column (rest, ty) ->
      let build, index = get_columns rest row in
      build (get ty row index), index + 1

  let query ~sql request =
    Sqlgg_query.make ~sql ~name:request.name ~kind:request.kind ()

  let set_params request statement =
    let pending = ref [] in
    request.params { bind = (fun ty value -> pending := (fun params -> bind ty params value) :: !pending) };
    let params = T.start_params statement (List.length !pending) in
    List.iter (fun bind -> bind params) (List.rev !pending);
    T.finish_params params

  let get_row columns row = fst (get_columns columns row)

  let sql : type cardinality. (_, cardinality) t -> string option =
   fun request ->
    match request.sql literals with
    | Sql sql -> Some sql
    | Nothing_to_run -> None

  let find_opt db (request : (_, zero_or_one) t) =
    match request.sql literals with
    | Sql sql ->
      T.select_one_maybe db (query ~sql request) (set_params request) (get_row request.result)

  let find db (request : (_, exactly_one) t) =
    match request.sql literals with
    | Sql sql ->
      T.select_one db (query ~sql request) (set_params request) (get_row request.result)

  let iter db (request : (_, many) t) callback =
    match request.sql literals with
    | Sql sql ->
      T.select db (query ~sql request) (set_params request)
        (fun row -> callback (get_row request.result row))

  let execute db (request : (_, affected_rows) t) =
    match request.sql literals with
    | Sql sql -> T.execute db (query ~sql request) (set_params request)
    | Nothing_to_run -> T.IO.return { T.affected_rows = 0L; insert_id = None }
end
