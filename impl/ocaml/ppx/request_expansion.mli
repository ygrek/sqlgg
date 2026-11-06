(** [[%sql.Request {| ... |}]]: a driver-independent [Sqlgg_request.t] value
    built from one checked statement. Unsupported statements are reported at
    [statement]. *)

val expand :
  loc:Ppxlib.Location.t ->
  statement:Ppxlib.Location.t ->
  Sqlgg_ir.Query.named ->
  Ppxlib.expression
