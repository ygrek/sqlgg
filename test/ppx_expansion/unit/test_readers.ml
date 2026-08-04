open OUnit

type voter = { vid : int64; note : string option } [@@deriving sqlgg ~nullable_cols]

type single = { only : int64 } [@@deriving sqlgg ~nullable_cols]

type quiet = { qid : int64; hush : string option [@sqlgg.map Fun.id] }
[@@deriving sqlgg ~nullable_cols]

type counted = {
  kid : int; [@sqlgg.col "k_id"] [@sqlgg.map Int64.to_int]
  seen : int; [@sqlgg.by] [@sqlgg.map Int64.to_int]
  tag : string; [@sqlgg.col "t"] [@sqlgg.default "none"]
}
[@@deriving sqlgg ~nullable_cols]

type holder = { hid : int64; counted : counted option [@sqlgg.nested] }
[@@deriving sqlgg]

type open_conv = { oid : int64; n : int [@sqlgg.by] }
[@@deriving sqlgg ~nullable_cols]

type joined = { jid : int64; voter : voter option [@sqlgg.nested] } [@@deriving sqlgg]

type joined_lax = { jid : int64; voter : voter option [@sqlgg.nested default_none] }
[@@deriving sqlgg]

type alone = { aid : int64; single : single option [@sqlgg.nested] } [@@deriving sqlgg]

type both = {
  bid : int64;
  voter : voter option; [@sqlgg.nested]
  single : single option; [@sqlgg.nested]
}
[@@deriving sqlgg]

let col v =
  { Sqlgg_scope.set = (fun () -> ())
  ; read = (fun () i -> v, i + 1)
  ; column = "c"
  ; count = 0
  ; deps = []
  }

let read c = fst (c.Sqlgg_scope.read () 0)

let cols ~vid ~note ~only =
  object
    method jid = col 1L
    method aid = col 1L
    method bid = col 1L
    method vid = col vid
    method note = col note
    method only = col only
  end

let quiet_cols ~qid ~hush =
  object
    method qid = col qid
    method hush = col hush
  end

let counted_cols ~kid ~seen ~tag =
  object
    method hid = col 1L
    method k_id = col kid
    method seen = col seen
    method t = col tag
  end

let open_conv_cols ~oid ~n =
  object
    method oid = col oid
    method n = col n
  end

let voter = function
  | None -> "none"
  | Some v -> Printf.sprintf "%Ld/%s" v.vid (Stdlib.Option.value v.note ~default:"-")

let single = function None -> "none" | Some s -> Int64.to_string s.only

let nested c = voter (read (joined_of_cols c)).voter
let default_none c = voter (read (joined_lax_of_cols c)).voter
let one c = single (read (alone_of_cols c)).single

let hushed c =
  match read (quiet_of_nullable_cols_exn c) with
  | None -> "none"
  | Some q -> Printf.sprintf "%Ld/%s" q.qid (Stdlib.Option.value q.hush ~default:"-")

let counted c =
  match (read (holder_of_cols c)).counted with
  | None -> "none"
  | Some c -> Printf.sprintf "%d/%d/%s" c.kid c.seen c.tag

let open_conv c =
  match read (open_conv_of_nullable_cols_exn ~n:Int64.to_int c) with
  | None -> "none"
  | Some r -> Printf.sprintf "%Ld/%d" r.oid r.n

let side_by_side c =
  let r = read (both_of_cols c) in
  voter r.voter ^ " " ^ single r.single

let case name got want =
  name >:: fun () ->
  let outcome = try got () with Failure w -> "raises " ^ w in
  assert_equal ~printer:(fun s -> s) want outcome

let suite =
  "relation readers"
  >::: [ case "an optional column proves the relation"
           (fun () -> nested (cols ~vid:None ~note:(Some "x") ~only:None))
           "raises sqlgg: voter.vid is NULL"
       ; case "an optional column proves it, lax"
           (fun () -> default_none (cols ~vid:None ~note:(Some "x") ~only:None))
           "none"
       ; case "a converted column does not prove it"
           (fun () -> hushed (quiet_cols ~qid:None ~hush:(Some "x")))
           "none"
       ; case "an optional column may be absent"
           (fun () -> nested (cols ~vid:(Some 5L) ~note:None ~only:None))
           "5/-"
       ; case "an open conversion reads on its own"
           (fun () -> open_conv (open_conv_cols ~oid:(Some 4L) ~n:(Some 9L)))
           "4/9"
       ; case "an open conversion, relation absent"
           (fun () -> open_conv (open_conv_cols ~oid:None ~n:None))
           "none"
       ; case "col, map, by and default together"
           (fun () ->
             counted (counted_cols ~kid:(Some 10L) ~seen:(Some 3L) ~tag:(Some "x")))
           "10/3/x"
       ; case "the default fills in"
           (fun () -> counted (counted_cols ~kid:(Some 10L) ~seen:(Some 3L) ~tag:None))
           "10/3/none"
       ; case "converted columns are witnesses"
           (fun () -> counted (counted_cols ~kid:None ~seen:(Some 3L) ~tag:None))
           "raises sqlgg: counted.kid is NULL"
       ; case "a converted relation can be absent"
           (fun () -> counted (counted_cols ~kid:None ~seen:None ~tag:None))
           "none"
       ; case "the only column is the witness"
           (fun () -> one (cols ~vid:None ~note:None ~only:(Some 7L)))
           "7"
       ; case "the only column is NULL"
           (fun () -> one (cols ~vid:None ~note:None ~only:None))
           "none"
       ; case "relations stand on their own"
           (fun () -> side_by_side (cols ~vid:(Some 5L) ~note:None ~only:None))
           "5/- none"
       ; case "both relations absent"
           (fun () -> side_by_side (cols ~vid:None ~note:None ~only:None))
           "none none"
       ]

let () = ignore (run_test_tt_main suite)
