open Printf

module M = Matrix.Sqlgg(Print_ocaml_impl)
module R = Reusable.Sqlgg(Print_ocaml_impl)

let go name f =
  printf "-- %s\n" name;
  Print_ocaml_impl.clear_mock_responses ();
  Print_ocaml_impl.setup_select_response [];
  f ();
  printf "\n"

let () =
  let db = () in

  go "before_param" (fun () ->
    M.Before_param.select db ~st:111L M.Before_param.name (fun _ -> ()));

  go "before_in: empty" (fun () ->
    M.Before_in.select db ~ids:[] M.Before_in.name (fun _ -> ()));
  go "before_in: two" (fun () ->
    M.Before_in.select db ~ids:[111L; 112L] M.Before_in.name (fun _ -> ()));

  go "before_not_in: empty" (fun () ->
    M.Before_not_in.select db ~ids:[] M.Before_not_in.name (fun _ -> ()));
  go "before_not_in: two" (fun () ->
    M.Before_not_in.select db ~ids:[111L; 112L] M.Before_not_in.name (fun _ -> ()));

  go "before_tuple: empty" (fun () ->
    M.Before_tuple.select db ~pairs:[] M.Before_tuple.name (fun _ -> ()));
  go "before_tuple: two" (fun () ->
    M.Before_tuple.select db ~pairs:[(111L, "a"); (112L, "b")] M.Before_tuple.name (fun _ -> ()));

  go "before_opt: none" (fun () ->
    M.Before_opt.select db ~st:None M.Before_opt.name (fun _ -> ()));
  go "before_opt: some" (fun () ->
    M.Before_opt.select db ~st:(Some 111L) M.Before_opt.name (fun _ -> ()));

  go "before_choice: all" (fun () ->
    M.Before_choice.select db ~c:`All M.Before_choice.name (fun _ -> ()));
  go "before_choice: by status" (fun () ->
    M.Before_choice.select db ~c:(`ByS 111L) M.Before_choice.name (fun _ -> ()));

  go "inside_param: plain col" (fun () ->
    M.Inside_param.select db M.Inside_param.id (fun _ -> ()));
  go "inside_param: param col" (fun () ->
    M.Inside_param.select db (M.Inside_param.is_new 222L) (fun _ -> ()));

  go "inside_in: empty" (fun () ->
    M.Inside_in.select db (M.Inside_in.hit []) (fun _ -> ()));
  go "inside_in: two" (fun () ->
    M.Inside_in.select db (M.Inside_in.hit [221L; 222L]) (fun _ -> ()));

  go "inside_not_in: empty" (fun () ->
    M.Inside_not_in.select db (M.Inside_not_in.miss []) (fun _ -> ()));
  go "inside_not_in: two" (fun () ->
    M.Inside_not_in.select db (M.Inside_not_in.miss [221L; 222L]) (fun _ -> ()));

  go "inside_tuple: empty" (fun () ->
    M.Inside_tuple.select db (M.Inside_tuple.matched []) (fun _ -> ()));
  go "inside_tuple: two" (fun () ->
    M.Inside_tuple.select db (M.Inside_tuple.matched [(221L, "a"); (222L, "b")]) (fun _ -> ()));

  go "inside_choice: zero" (fun () ->
    M.Inside_choice.select db (M.Inside_choice.x `Zero) (fun _ -> ()));
  go "inside_choice: val" (fun () ->
    M.Inside_choice.select db (M.Inside_choice.x (`Val 222L)) (fun _ -> ()));

  go "after_param" (fun () ->
    M.After_param.select db M.After_param.name ~st:333L (fun _ -> ()));

  go "after_in: empty" (fun () ->
    M.After_in.select db M.After_in.name ~ids:[] (fun _ -> ()));
  go "after_in: two" (fun () ->
    M.After_in.select db M.After_in.name ~ids:[333L; 334L] (fun _ -> ()));

  go "after_not_in: empty" (fun () ->
    M.After_not_in.select db M.After_not_in.name ~ids:[] (fun _ -> ()));
  go "after_not_in: two" (fun () ->
    M.After_not_in.select db M.After_not_in.name ~ids:[333L; 334L] (fun _ -> ()));

  go "after_tuple: empty" (fun () ->
    M.After_tuple.select db M.After_tuple.name ~pairs:[] (fun _ -> ()));
  go "after_tuple: two" (fun () ->
    M.After_tuple.select db M.After_tuple.name ~pairs:[(333L, "a"); (334L, "b")] (fun _ -> ()));

  go "after_opt: none" (fun () ->
    M.After_opt.select db M.After_opt.name ~st:None (fun _ -> ()));
  go "after_opt: some" (fun () ->
    M.After_opt.select db M.After_opt.name ~st:(Some 333L) (fun _ -> ()));

  go "after_choice: all" (fun () ->
    M.After_choice.select db M.After_choice.name ~c:`All (fun _ -> ()));
  go "after_choice: by status" (fun () ->
    M.After_choice.select db M.After_choice.name ~c:(`ByS 333L) (fun _ -> ()));

  go "shared_before_after: all" (fun () ->
    M.Shared_before_after.select db M.Shared_before_after.name ~c:`All (fun _ -> ()));
  go "shared_before_after: by status" (fun () ->
    M.Shared_before_after.select db M.Shared_before_after.name ~c:(`ByS 123L) (fun _ -> ()));

  go "order_before_inside_after: plain col" (fun () ->
    M.Order_before_inside_after.select db ~a:111L M.Order_before_inside_after.id ~c:333L (fun _ -> ()));
  go "order_before_inside_after: param col" (fun () ->
    M.Order_before_inside_after.select db ~a:111L (M.Order_before_inside_after.flag 222L) ~c:333L (fun _ -> ()));

  go "mega: everything on" (fun () ->
    M.Mega.select db ~ids:[111L; 112L] ~st:(Some 113L) (M.Mega.is_new 222L)
      ~pairs:[(333L, "a")] ~c:(`ByS 331L) ~sort:`N (fun _ -> ()));
  go "mega: everything off" (fun () ->
    M.Mega.select db ~ids:[] ~st:None M.Mega.id
      ~pairs:[] ~c:`All ~sort:`I (fun _ -> ()));

  go "reuse_param" (fun () ->
    R.Reuse_param.select db ~st:111L R.Reuse_param.name ~lo:333L (fun _ -> ()));

  go "reuse_param_col: plain col" (fun () ->
    R.Reuse_param_col.select db ~st:111L R.Reuse_param_col.id (fun _ -> ()));
  go "reuse_param_col: param col" (fun () ->
    R.Reuse_param_col.select db ~st:111L (R.Reuse_param_col.is_new 222L) (fun _ -> ()));

  go "reuse_mixed: everything on" (fun () ->
    R.Reuse_mixed.select db ~ids:[111L; 112L] ~st:(Some 113L) ~c:(`ByS 114L)
      (R.Reuse_mixed.name) ~pairs:[(333L, "a")] ~sort:`N (fun _ -> ()));
  go "reuse_mixed: everything off" (fun () ->
    R.Reuse_mixed.select db ~ids:[] ~st:None ~c:`All
      (R.Reuse_mixed.id) ~pairs:[] ~sort:`I (fun _ -> ()));

  go "reuse_twice: same fragment binds both ctes" (fun () ->
    R.Reuse_twice.select db ~st:111L R.Reuse_twice.name ~lo:333L (fun _ -> ()))
