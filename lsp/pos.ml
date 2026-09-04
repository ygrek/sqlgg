open Sqlgg

type t = Sql.pos

let contains ((start, stop) : t) offset = offset >= start && offset < stop

let covers ((start, stop) : t) offset = offset >= start && offset <= stop

let shift n (start, stop) = (n + start, n + stop)

let is_empty ((start, stop) : t) = stop <= start

let valid_offset text offset = Int.max 0 (Int.min (String.length text) offset)

let find_innermost_by includes offset candidates =
  let width (_, (start, stop)) = stop - start in
  Seq.fold_left (fun best ((_, pos) as candidate) ->
    if not (includes pos offset) then best
    else
      match best with
      | Some found when width found <= width candidate -> best
      | Some _ | None -> Some candidate) None candidates

let find_innermost offset candidates =
  find_innermost_by contains offset candidates

let find_innermost_covering offset candidates =
  find_innermost_by covers offset candidates
