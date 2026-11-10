val relocate : loc:Ppxlib.Location.t -> Ppxlib.Ast_traverse.map

val expand :
  loc:Ppxlib.Location.t ->
  statement:Ppxlib.Location.t ->
  Sqlgg_ir.Query.named ->
  Ppxlib.expression
