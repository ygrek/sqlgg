module Priority : sig
  type t
  val order : t -> int
end

type item = {
  label : string;
  detail : string;
  kind : Linol_lsp.Types.CompletionItemKind.t;
  priority : Priority.t;
}

val at : ?cache:Document.Cache.t -> path:string -> string -> int -> Pos.t * item list
