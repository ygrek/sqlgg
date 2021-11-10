
let replace_all_chars ~str ~sub ~by =
  let rec get_chunks s sub acc off =
    if off >= String.length s then List.rev ("" :: acc)
    else
      match String.index_from_opt s off sub with
      | None -> List.rev (String.sub s off (String.length s - off) :: acc)
      | Some off' -> get_chunks s sub (String.sub s off (off' - off) :: acc) (off' + 1)
  in
  String.concat by (get_chunks str sub [] 0)
