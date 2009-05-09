(* $Id$ *)

open Printf

module Type =
struct
  type t = | Int | Text | Blob | Float | Bool
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

