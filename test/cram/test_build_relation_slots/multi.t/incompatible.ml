module Q = Multi.Sqlgg(Print_impl)
let _ = Q.Pair.select () ~left_src:Q.Pair.Left_src.table_users ~right_src:Q.Pair.Right_src.table_tags
