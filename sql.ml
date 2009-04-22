(* $Id$ *)

open Printf

module Type =
struct
  type t = | Int | Text | Blob 
        deriving (Show)

  let to_string = Show.show<t>
(*   let to_string = function | Int -> "Int" | Text -> "Text" | Blob -> "Blob" *)
  let to_cpp_string x = "typename Traits::" ^ (to_string x)
end

module Constraint =
struct
  type conflict_algo = | Ignore | Replace | Abort | Fail | Rollback
       deriving (Show)

  type t = | PrimaryKey | NotNull | Unique | Autoincrement | OnConflict of conflict_algo
       deriving (Show)
end

module Col =
struct
  type t = {name:string; cpp_name:string; sqltype:Type.t; constraints:Constraint.t list}
       deriving (Show)
  let make name sqltype constraints = 
    {name=name; cpp_name=name; sqltype=sqltype; constraints=constraints}
  let type_to_string c = Type.to_string c.sqltype
  let type_to_cpp_string c = Type.to_cpp_string c.sqltype
  let is_primary_key c = List.mem Constraint.PrimaryKey c.constraints
end

module Table =
struct
  type t = {name:string; cpp_name:string; cols:Col.t list}
       deriving (Show)

  let make name cols = {name=name;cpp_name=name;cols=cols}
end
