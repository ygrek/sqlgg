type channel = {
  channel_id : int64;
  channel_name : string;
  image_url : string option;
}
[@@deriving sqlgg ~nullable_cols]

type post = {
  id : int64;
  body : string option;
  channel : channel option; [@sqlgg.nested]
}
[@@deriving sqlgg]

module Db = Posts.Sqlgg (Print_impl)

let () =
  let open Print_impl in
  let open Db.Feed in
  clear_mock_responses ();
  setup_select_response
    [ make_mock_row
        [ mock_int 1L; mock_text "hi"; mock_int 10L; mock_text "ocaml"
        ; mock_text "pic"
        ]
    ];
  match (Stdlib.List.hd (List.select () (post_of_cols cols) ~min_id:0L)).channel with
  | None -> print_endline "channel none"
  | Some c ->
    Printf.printf "channel %Ld/%s img=%s\n" c.channel_id c.channel_name
      (Stdlib.Option.value c.image_url ~default:"none")
