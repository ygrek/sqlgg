type zero_or_one
type exactly_one
type many
type affected_rows

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

module Make (T : Sqlgg_traits.M_io) : sig
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

  val sql : ('output, types, 'cardinality) t -> string option

  val find_opt :
    [> `RO ] T.connection -> ('output, types, zero_or_one) t -> 'output option T.IO.future

  val find : [> `RO ] T.connection -> ('output, types, exactly_one) t -> 'output T.IO.future

  val iter :
    [> `RO ] T.connection -> ('output, types, many) t -> ('output -> unit) -> unit T.IO.future

  val execute :
    [> `WR ] T.connection -> (unit, types, affected_rows) t -> T.execute_response T.IO.future
end
