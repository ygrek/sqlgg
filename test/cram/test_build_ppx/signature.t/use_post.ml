let read
    (cols :
      < post_id : (int64, _, _, _) Sqlgg_scope.col
      ; body : (string option, _, _, _) Sqlgg_scope.col
      ; channel_id : (int64, _, _, _) Sqlgg_scope.col
      ; channel_name : (string option, _, _, _) Sqlgg_scope.col >) =
  M.post_of_cols cols
