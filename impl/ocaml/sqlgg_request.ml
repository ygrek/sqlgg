type zero_or_one = |
type exactly_one = |
type many = |
type affected_rows = |

type 'types bool_of = 'a constraint 'types = < bool : 'a ; .. >
type 'types int_of = 'a constraint 'types = < int : 'a ; .. >
type 'types float_of = 'a constraint 'types = < float : 'a ; .. >
type 'types decimal_of = 'a constraint 'types = < decimal : 'a ; .. >
type 'types text_of = 'a constraint 'types = < text : 'a ; .. >
type 'types datetime_of = 'a constraint 'types = < datetime : 'a ; .. >
type 'types json_of = 'a constraint 'types = < json : 'a ; .. >
type 'types json_path_of = 'a constraint 'types = < json_path : 'a ; .. >
type 'types any_of = 'a constraint 'types = < any : 'a ; .. >

type (_, _) scalar =
  | Bool : ('types bool_of, 'types) scalar
  | Int : ('types int_of, 'types) scalar
  | UInt64 : (Unsigned.UInt64.t, 'types) scalar
  | Float : ('types float_of, 'types) scalar
  | Decimal : ('types decimal_of, 'types) scalar
  | Text : ('types text_of, 'types) scalar
  | Blob : (string, 'types) scalar
  | Datetime : ('types datetime_of, 'types) scalar
  | Json : ('types json_of, 'types) scalar
  | Json_path : ('types json_path_of, 'types) scalar
  | Any : ('types any_of, 'types) scalar
  | One_or_all : (Sqlgg_trait_types.one_or_all, 'types) scalar
  | Enum : (module Sqlgg_traits.Enum with type t = 'a) -> ('a, 'types) scalar

type canonical =
  < bool : bool
  ; int : int64
  ; float : float
  ; decimal : float
  ; text : string
  ; datetime : float
  ; json : Sqlgg_trait_types.json
  ; json_path : string
  ; any : string >

type (_, _) representation =
  | Native : ('types, 'types) representation
  | Canonical : ('types, canonical) representation

type (_, _) ty =
  | Required : ('types, 'repr) representation * ('a, 'repr) scalar -> ('a, 'types) ty
  | Optional : ('types, 'repr) representation * ('a, 'repr) scalar -> ('a option, 'types) ty

type 'types binder = {
  bind : 'a 'repr. ('types, 'repr) representation -> ('a, 'repr) scalar -> 'a -> unit;
  null : unit -> unit;
}

type 'types literals = {
  literal : 'a 'repr. ('types, 'repr) representation -> ('a, 'repr) scalar -> 'a -> string;
}

type (_, _) columns =
  | Build : 'build -> ('build, 'types) columns
  | Column : ('x -> 'build, 'types) columns * ('x, 'types) ty -> ('build, 'types) columns

type _ outcome =
  | Sql : string -> 'cardinality outcome
  | Nothing_to_run : affected_rows outcome

type ('output, 'types, 'cardinality) t = {
  sql : 'types literals -> 'cardinality outcome;
  name : string;
  kind : Sqlgg_query.kind;
  param_count : int;
  params : 'types binder -> unit;
  result : ('output, 'types) columns;
}

module Make (T : Sqlgg_traits.M_io) = struct
  type types =
    < bool : T.Types.Bool.t
    ; int : T.Types.Int.t
    ; float : T.Types.Float.t
    ; decimal : T.Types.Decimal.t
    ; text : T.Types.Text.t
    ; datetime : T.Types.Datetime.t
    ; json : T.Types.Json.t
    ; json_path : T.Types.Json_path.t
    ; any : T.Types.Any.t >

  type 'a scalar_ops = {
    bind : T.params -> 'a -> unit;
    literal : 'a -> string;
    get : T.row -> int -> 'a;
    get_nullable : T.row -> int -> 'a option;
  }

  let ops : type a repr. (types, repr) representation -> (a, repr) scalar -> a scalar_ops =
   fun representation scalar ->
    match representation with
    | Native ->
      (match (scalar : (a, types) scalar) with
       | Bool ->
         { bind = T.set_param_Bool; literal = T.Types.Bool.to_literal;
           get = T.get_column_Bool; get_nullable = T.get_column_Bool_nullable }
       | Int ->
         { bind = T.set_param_Int; literal = T.Types.Int.to_literal;
           get = T.get_column_Int; get_nullable = T.get_column_Int_nullable }
       | UInt64 ->
         { bind = T.set_param_UInt64; literal = T.Types.UInt64.uint64_to_literal;
           get = T.get_column_UInt64; get_nullable = T.get_column_UInt64_nullable }
       | Float ->
         { bind = T.set_param_Float; literal = T.Types.Float.to_literal;
           get = T.get_column_Float; get_nullable = T.get_column_Float_nullable }
       | Decimal ->
         { bind = T.set_param_Decimal; literal = T.Types.Decimal.to_literal;
           get = T.get_column_Decimal; get_nullable = T.get_column_Decimal_nullable }
       | Text ->
         { bind = T.set_param_Text; literal = T.Types.Text.to_literal;
           get = T.get_column_Text; get_nullable = T.get_column_Text_nullable }
       | Blob ->
         { bind = T.set_param_string; literal = T.Types.Blob.string_to_literal;
           get = T.get_column_string; get_nullable = T.get_column_string_nullable }
       | Datetime ->
         { bind = T.set_param_Datetime; literal = T.Types.Datetime.to_literal;
           get = T.get_column_Datetime; get_nullable = T.get_column_Datetime_nullable }
       | Json ->
         { bind = T.set_param_Json; literal = T.Types.Json.to_literal;
           get = T.get_column_Json; get_nullable = T.get_column_Json_nullable }
       | Json_path ->
         { bind = T.set_param_Json_path; literal = T.Types.Json_path.to_literal;
           get = T.get_column_Json_path; get_nullable = T.get_column_Json_path_nullable }
       | Any ->
         { bind = T.set_param_Any; literal = T.Types.Any.to_literal;
           get = T.get_column_Any; get_nullable = T.get_column_Any_nullable }
       | One_or_all ->
         { bind = T.set_param_one_or_all; literal = T.Types.One_or_all.to_literal;
           get = T.get_column_one_or_all; get_nullable = T.get_column_one_or_all_nullable }
       | Enum (module E) ->
         let module Codec = T.Make_enum (E) in
         { bind = Codec.set_param; literal = Codec.to_literal;
           get = Codec.get_column; get_nullable = Codec.get_column_nullable })
    | Canonical ->
      (match (scalar : (a, canonical) scalar) with
       | Bool ->
         { bind = T.set_param_bool; literal = T.Types.Bool.bool_to_literal;
           get = T.get_column_bool; get_nullable = T.get_column_bool_nullable }
       | Int ->
         { bind = T.set_param_int64; literal = T.Types.Int.int64_to_literal;
           get = T.get_column_int64; get_nullable = T.get_column_int64_nullable }
       | UInt64 ->
         { bind = T.set_param_uint64; literal = T.Types.UInt64.uint64_to_literal;
           get = T.get_column_uint64; get_nullable = T.get_column_uint64_nullable }
       | Float ->
         { bind = T.set_param_float; literal = T.Types.Float.float_to_literal;
           get = T.get_column_float; get_nullable = T.get_column_float_nullable }
       | Decimal ->
         { bind = T.set_param_float; literal = T.Types.Decimal.float_to_literal;
           get = T.get_column_float; get_nullable = T.get_column_float_nullable }
       | Text ->
         { bind = T.set_param_string; literal = T.Types.Text.string_to_literal;
           get = T.get_column_string; get_nullable = T.get_column_string_nullable }
       | Blob ->
         { bind = T.set_param_string; literal = T.Types.Blob.string_to_literal;
           get = T.get_column_string; get_nullable = T.get_column_string_nullable }
       | Datetime ->
         { bind = T.set_param_float; literal = T.Types.Datetime.float_to_literal;
           get = T.get_column_float; get_nullable = T.get_column_float_nullable }
       | Json ->
         { bind = T.set_param_json; literal = T.Types.Json.json_to_literal;
           get = T.get_column_json; get_nullable = T.get_column_json_nullable }
       | Json_path ->
         { bind = T.set_param_string; literal = T.Types.Text.string_to_literal;
           get = T.get_column_string; get_nullable = T.get_column_string_nullable }
       | Any ->
         { bind = T.set_param_string; literal = T.Types.Text.string_to_literal;
           get = T.get_column_string; get_nullable = T.get_column_string_nullable }
       | One_or_all ->
         { bind = T.set_param_one_or_all; literal = T.Types.One_or_all.to_literal;
           get = T.get_column_one_or_all; get_nullable = T.get_column_one_or_all_nullable }
       | Enum (module E) ->
         let module Codec = T.Make_enum (E) in
         { bind = Codec.set_param; literal = Codec.to_literal;
           get = Codec.get_column; get_nullable = Codec.get_column_nullable })

  let literals = { literal = (fun representation scalar -> (ops representation scalar).literal) }

  let reader : type output. (output, types) columns -> (T.row -> output) =
    let get_column : type a. (a, types) ty -> int -> (T.row -> a) =
     fun ty index ->
      match ty with
      | Required (representation, scalar) ->
        let ops = ops representation scalar in
        fun row -> ops.get row index
      | Optional (representation, scalar) ->
        let ops = ops representation scalar in
        fun row -> ops.get_nullable row index
    in
    let rec compile : type build. (build, types) columns -> int -> (T.row -> build) * int =
     fun columns index ->
      match columns with
      | Build build -> (fun _row -> build), index
      | Column (rest, ty) ->
        let read_rest, next = compile rest index in
        let read_value = get_column ty next in
        (fun row -> let build = read_rest row in build (read_value row)), next + 1
    in
    fun columns -> fst (compile columns 0)

  let query ~sql request =
    Sqlgg_query.make ~sql ~name:request.name ~kind:request.kind ()

  let set_params request statement =
    let params = T.start_params statement request.param_count in
    request.params
      { bind = (fun representation scalar value -> (ops representation scalar).bind params value);
        null = (fun () -> T.set_param_null params) };
    T.finish_params params

  let sql : type cardinality. (_, types, cardinality) t -> string option =
   fun request ->
    match request.sql literals with
    | Sql sql -> Some sql
    | Nothing_to_run -> None

  let find_opt db (request : (_, types, zero_or_one) t) =
    match request.sql literals with
    | Sql sql ->
      T.select_one_maybe db (query ~sql request) (set_params request) (reader request.result)

  let find db (request : (_, types, exactly_one) t) =
    match request.sql literals with
    | Sql sql ->
      T.select_one db (query ~sql request) (set_params request) (reader request.result)

  let iter db (request : (_, types, many) t) callback =
    match request.sql literals with
    | Sql sql ->
      let read = reader request.result in
      T.select db (query ~sql request) (set_params request) (fun row -> callback (read row))

  let execute db (request : (_, types, affected_rows) t) =
    match request.sql literals with
    | Sql sql -> T.execute db (query ~sql request) (set_params request)
    | Nothing_to_run -> T.IO.return { T.affected_rows = 0L; insert_id = None }
end
