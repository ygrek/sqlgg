
type outcome =
  | Executable of Syntax.result
  | Reusable of Parser.parse_result
  | Verbatim
  | Not_reusable

val statement : dynamic_select:Props.dynamic_select -> Statements.t -> outcome

val reset : unit -> unit

type state
val snapshot : unit -> state
val restore : state -> unit
