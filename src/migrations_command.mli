

type host_language = Ocaml

type generation = {
  dialect : [ `MySQL | `TiDB ];
  host_language : host_language;
  schema_file : string;
  migrations_file : string;
  manual_file : string option;
  module_name : string;
  now : int option;
}

type request =
  | Generate of generation
  | Print_dune_rules of {
      generation : generation;
      generated_file : string;
    }

val parse : string array -> [ `Request of request | `Exit of int ]

val generate_arguments : generation -> string list
