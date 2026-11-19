include Print_impl

type bound_statement = {
  sql : string;
  params : string list;
}

let captured_bound_statements = ref []

let reset_bound_statements () =
  captured_bound_statements := []

let bound_statements () =
  List.rev !captured_bound_statements

let finish_params
    ((statement : statement), params, index, declared) =
  if !index <> declared then
    raise
      (Oops
         (Printf.sprintf
            "declared %d parameters, bound %d"
            declared
            !index));
  captured_bound_statements :=
    {
      sql = statement.sql;
      params = List.rev !params;
    }
    :: !captured_bound_statements
