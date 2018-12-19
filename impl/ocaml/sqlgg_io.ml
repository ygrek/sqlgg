(**
  OCaml IO signature for sqlgg
  by Raman Varabets
  2018-12-19

  This is free and unencumbered software released into the public domain.

  Anyone is free to copy, modify, publish, use, compile, sell, or
  distribute this software, either in source code form or as a compiled
  binary, for any purpose, commercial or non-commercial, and by any
  means.

  For more information, please refer to <http://unlicense.org/>
*)

module type M = sig
  type 'a future
  val return : 'a -> 'a future
  val (>>=) : 'a future -> ('a -> 'b future) -> 'b future
end

module Blocking : M with type 'a future = 'a = struct
  type 'a future = 'a
  let return x = x
  let (>>=) x f = f x
end
