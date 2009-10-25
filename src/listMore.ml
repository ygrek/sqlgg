(* *)

open Prelude

module List = 
struct

  include ExtList.List

  let filter_valid lst =
    (*List.fold_left (fun l elem -> match elem with | Some x -> x::l | None -> l) [] (List.rev lst)*)
    filter_map id lst

  let find_option f = catch (find f)

end (* List *)
