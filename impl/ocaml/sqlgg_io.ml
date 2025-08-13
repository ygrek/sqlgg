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
  val bracket : 'a future -> ('a -> unit future) -> ('a -> 'b future) -> 'b future
  val catch : (unit -> 'a future) -> (exn -> 'a future) -> 'a future
  val try_bind : (unit -> 'a future) -> ('a -> 'b future) -> (exn -> 'b future) -> 'b future
  val async : (unit -> unit future) -> unit

  module List : sig
    val iter_s : ('a -> unit future) -> 'a list -> unit future
  end
end

module Blocking : M with type 'a future = 'a = struct

  type 'a future = 'a

  let return x = x

  let (>>=) x f = f x

  let bracket x dtor f =
    let r = try f x with exn -> dtor x; raise exn in
    dtor x;
    r

  let catch f h =
    try f () with exn -> h exn

  let try_bind f succ_handler exc_handler =
    try
      let x = f () in
      succ_handler x
    with exn -> exc_handler exn

  let async f = ignore (f ())

  module List = struct
    let rec iter_s f = function
      | [] -> return ()
      | x :: xs ->
          f x >>= fun () ->
          iter_s f xs 
  end
end
