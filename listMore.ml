(* *)

open Operators

module List = 
struct

  include ExtList.List

  let filter_valid lst =
    (*List.fold_left (fun l elem -> match elem with | Some x -> x::l | None -> l) [] (List.rev lst)*)
    filter_map id lst

  let find_option f l = try Some (find f l) with Not_found -> None

end (* List *)
