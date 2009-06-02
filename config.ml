(**  *)

open ListMore
open ExtString
open Operators

let version_number = "0.2.1"

let version = Printf.sprintf "%s (%s)"
    version_number
    (Git.revision >> String.explode >> List.take 8 >> String.implode)

