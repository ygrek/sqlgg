type content = { text : string option [@sqlgg.col "body"] } [@@deriving sqlgg]

type post = {
  id : int64;
  content : content; [@sqlgg.nested]
  reply_count : int; [@sqlgg.map Int64.to_int] [@sqlgg.set Int64.of_int]
  hits : int64; [@sqlgg.default 0L] [@sqlgg.set (fun h -> Some h)]
}
[@@deriving sqlgg]

type counted = { id : int64; n : int [@sqlgg.by] } [@@deriving sqlgg]

type scaled = { id : int64; n : int [@sqlgg.by] [@sqlgg.map Int64.to_int] }
[@@deriving sqlgg]

module Db = Posts.Sqlgg(Print_impl)

let run label f =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  f ();
  print_newline ()

let rows l =
  Print_impl.setup_select_response (Stdlib.List.map Print_impl.make_mock_row l)

let print_post (p : post) =
  Printf.printf "id=%Ld text=%s reply_count=%d hits=%Ld\n" p.id
    (Option.value p.content.text ~default:"NULL") p.reply_count p.hits

let print_counted (c : counted) = Printf.printf "id=%Ld n=%d\n" c.id c.n
let print_scaled (c : scaled) = Printf.printf "id=%Ld n=%d\n" c.id c.n

let () =
  let open Print_impl in
  let open Db.Feed in
  run "a post" (fun () ->
    rows [ [ mock_int 1L; mock_text "hi"; mock_int 7L; mock_null ] ];
    Stdlib.List.iter print_post (List.select () (post_of_cols cols) ~min_id:0L));
  run "the same post, columns picked by hand" (fun () ->
    rows [ [ mock_int 1L; mock_text "hi"; mock_int 7L; mock_null ] ];
    let col =
      post_of_cols_gen ~id:cols#id ~content:(content_of_cols cols)
        ~reply_count:(Sqlgg_scope.map Int64.to_int cols#reply_count)
        ~hits:(Sqlgg_scope.map (fun h -> Option.value h ~default:(-1L)) cols#hits)
    in
    Stdlib.List.iter print_post (List.select () col ~min_id:0L))

let () =
  let open Print_impl in
  run "n read as an int64" (fun () ->
    let open Db.Counts in
    rows [ [ mock_int 1L; mock_int 9L ] ];
    Stdlib.List.iter print_counted
      (List.select () (counted_of_cols ~n:Int64.to_int cols) ~min_id:0L));
  run "the same record, n read as text" (fun () ->
    let open Db.Labels in
    rows [ [ mock_int 1L; mock_text "abcd" ] ];
    let n s = String.length (Option.value s ~default:"") in
    Stdlib.List.iter print_counted
      (List.select () (counted_of_cols ~n cols) ~min_id:0L));
  run "conversion left to the default" (fun () ->
    let open Db.Counts in
    rows [ [ mock_int 1L; mock_int 9L ] ];
    Stdlib.List.iter print_scaled (List.select () (scaled_of_cols cols) ~min_id:0L));
  run "conversion passed at the call site" (fun () ->
    let open Db.Counts in
    rows [ [ mock_int 1L; mock_int 9L ] ];
    let n x = Int64.to_int x * 10 in
    Stdlib.List.iter print_scaled
      (List.select () (scaled_of_cols ~n cols) ~min_id:0L))

let () =
  run "inserting a post" (fun () ->
    Print_impl.setup_execute_response ~affected_rows:1L ();
    let p : post =
      { id = 5L; content = { text = Some "hello" }; reply_count = 2; hits = 11L }
    in
    ignore (post_apply (Db.add_post ()) p))
