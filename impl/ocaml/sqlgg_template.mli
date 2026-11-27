(** SQL text assembled at runtime by generated code.

  Numbered placeholders ([$1], [$2], ...) are assigned only when the complete
  text is rendered, so a part chosen at runtime (for example the relation of a
  relation slot) shifts the numbers of the placeholders that follow it. *)

type t

val text : string -> t

(** placeholder numbered by its position in the rendered text *)
val numbered_param : t

val concat : t list -> t

val render : t -> string
