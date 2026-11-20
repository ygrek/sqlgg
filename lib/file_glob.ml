type directory =
  | Entries of string array
  | Unreadable of string

type t = (string, directory) Hashtbl.t

let create () = Hashtbl.create 4

let expand cache path =
  if
    not
      (String.exists
         (function
           | '*' | '?' | '[' | ']' | '{' | '}' | '\\' -> true
           | _ -> false)
         (Filename.basename path))
  then Ok (if Sys.file_exists path then [ path ] else [])
  else
    match
      Re.Glob.glob_result ~anchored:true ~pathname:false ~period:false
        (Filename.basename path)
    with
    | Error `Parse_error -> Error `Malformed_pattern
    | Ok expression ->
      let directory = Filename.dirname path in
      let entries =
        match Hashtbl.find_opt cache directory with
        | Some cached -> cached
        | None ->
          let cached =
            match Sys.readdir directory with
            | entries -> Entries entries
            | exception Sys_error reason -> Unreadable reason
          in
          Hashtbl.replace cache directory cached;
          cached
      in
      match entries with
      | Unreadable reason -> Error (`Unreadable_directory reason)
      | Entries entries ->
        let pattern = Re.compile expression in
        Ok
          (Array.fold_left
             (fun matches entry ->
               if Re.execp pattern entry then entry :: matches else matches)
             [] entries
           |> List.sort String.compare
           |> List.map (Filename.concat directory))
