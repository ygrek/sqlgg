type ('a, 'brand, 'row, 'params) col = {
  set : 'params -> unit;
  read : 'row -> int -> 'a * int;
  column : string;
  count : int;
  deps : 'brand list;
}

module type Syntax = sig
  type 'a t
  val pure : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

module Syntax = struct
  let pure x = {
    set = (fun _p -> ());
    read = (fun _row idx -> (x, idx));
    column = "";
    count = 0;
    deps = [];
  }

  let apply f a = {
    set = (fun p -> f.set p; a.set p);
    read = (fun row idx ->
      let (vf, i1) = f.read row idx in
      let (va, i2) = a.read row i1 in
      (vf va, i2));
    column = begin match f.column, a.column with
      | "", c | c, "" -> c
      | c1, c2 -> c1 ^ ", " ^ c2
      end;
    count = f.count + a.count;
    deps = f.deps @ List.filter (fun d -> not (List.mem d f.deps)) a.deps;
  }

  let map f a = apply (pure f) a

  let ( let+ ) t f = map f t
  let ( and+ ) a b = apply (map (fun a b -> (a, b)) a) b
end

include Syntax

module Make (M : sig
  type brand
  type row
  type params
end) : Syntax with type 'a t = ('a, M.brand, M.row, M.params) col = struct
  type 'a t = ('a, M.brand, M.row, M.params) col
  include Syntax
end
