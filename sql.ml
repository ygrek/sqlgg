(** *)

module Type =
struct
  type t = | Int | Text | Blob | Float | Bool | Datetime
        deriving (Show)

  let to_string = Show.show<t>
end

module Constraint =
struct
  type conflict_algo = | Ignore | Replace | Abort | Fail | Rollback
       deriving (Show)

  type t = | PrimaryKey | NotNull | Unique | Autoincrement | OnConflict of conflict_algo
       deriving (Show)
end

