(** The relations each relation slot of a SELECT can be, and how the types of the statement depend on the choice *)

(** What the analysis reads of one evaluation; ['statement] is the evaluation itself *)
type 'statement evaluation = {
  statement : 'statement;
  schema : Sql.schema_column list;
  vars : Sql.var list;
  kind : Stmt.kind;
  uses : Relation_slot.use list; (** columns of open slots it uses *)
}

type 'statement t = {
  evaluation : 'statement evaluation;
    (** with the first relation of the first slot, types that do not depend on the choice resolved
        and the widest cardinality of all relations *)
  relation_slots : Relation_slot.t list;
  scope : Sql.table list -> Sql.table list; (** a slot alias offers the columns every relation of the slot has *)
}

(** [evaluate ~bound select]: the statement or a reusable query with the slot of [bound] bound to its relation,
    the others open; the slots of the statement, in source order *)
val run :
  evaluate:(bound:(Relation_slot.Name.t * Sql.schema) option -> Sql.select_full -> ('statement evaluation, exn) result) ->
  Sql.select_full ->
  Relation_slot.Name.t Sql.located * Relation_slot.Name.t Sql.located list ->
  'statement t
