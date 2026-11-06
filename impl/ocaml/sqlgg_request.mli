(** Driver-independent typed requests with canonical host types.

    Every SQL type has one OCaml representation, the same for every driver:
    [Int] is [int64], [Text] is [string], and so on. A request names only
    these canonical types, so its type never mentions a driver; a driver that
    stores values differently converts inside its [bind]/[get]. *)

(** {1 Cardinalities} *)

type zero_or_one = Zero_or_one
type exactly_one = Exactly_one
type many = Many
type affected_rows = Affected_rows

(** {1 Canonical types} *)

(** SQL types with a driver-independent OCaml representation. [Datetime] is
    the driver's textual form, as read back from a column; [Enum] is the
    closed set of values declared in the schema, as a polymorphic variant. *)
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

(** A column or parameter: [Optional] is SQL [NULL]-able, and cannot nest *)
type _ ty =
  | Required : 'a scalar -> 'a ty
  | Optional : 'a scalar -> 'a option ty

(** Receives the parameters of a request one by one, each at its own type;
    polymorphic, so one binder serves parameters of every type *)
type binder = { bind : 'a. 'a ty -> 'a -> unit }

(** Renders a value as an SQL literal, for statements that splice values into
    the text ([IN @ids], [VALUES @rows]). It is supplied by the driver's
    [Types], so quoting and escaping are the driver's, never the library's. *)
type literals = { literal : 'a. 'a ty -> 'a -> string }

(** The columns of a row and how they make up one OCaml value: [Build] holds a
    function taking the columns left to right, [Column] adds the next column's
    type. The value is [unit] for no columns, the bare value for one, a tuple
    for several. *)
type _ columns =
  | Build : 'build -> 'build columns
  | Column : ('x -> 'build) columns * 'x ty -> 'build columns

(** {1 Requests} *)

(** What a request runs: its SQL text, or nothing when a list that must not be
    empty is. [Nothing_to_run] is only possible for [affected_rows], where it
    stands for zero rows changed, so no other executor has to handle it. *)
type _ outcome =
  | Sql : string -> 'cardinality outcome
  | Nothing_to_run : affected_rows outcome

(** A statement with its parameters bound, built by [[%sql.Request]]: an
    expansion with parameters is a function from them, labelled by name, to
    this. ['cardinality] selects the executor. *)
type ('output, 'cardinality) t = {
  sql : literals -> 'cardinality outcome;
  name : string;
  kind : Sqlgg_query.kind;
  params : binder -> unit;
  result : 'output columns;
}

(** {1 Execution}

    [Make] runs requests on a traits module, converting between the driver's
    host types and the canonical ones. *)
module Make (T : Sqlgg_traits.M_io) : sig
  (** The SQL text the request runs with this driver's literals, or [None] when
      there is nothing to run *)
  val sql : ('output, 'cardinality) t -> string option

  val find_opt :
    [> `RO ] T.connection -> ('output, zero_or_one) t -> 'output option T.IO.future

  val find : [> `RO ] T.connection -> ('output, exactly_one) t -> 'output T.IO.future

  val iter :
    [> `RO ] T.connection -> ('output, many) t -> ('output -> unit) -> unit T.IO.future

  val execute :
    [> `WR ] T.connection -> (unit, affected_rows) t -> T.execute_response T.IO.future
end
