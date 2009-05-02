(** *)

open Printf
open ExtString
open ListMore

type param_id = | Named of string | Numbered of int | Next deriving (Show)
type param_type = Sql.Type.t option deriving (Show)
type param = param_id * param_type deriving (Show)
type params = param list deriving (Show)

let params_to_string ps = Show.show<params>(ps)

type kind = | Select
            | Insert of string (** table name *)
            | Create of string (** table name *)
            | Update of string (** table name *)
            | Delete of string (** table name *)
            deriving (Show)

