module Q = Basic.Sqlgg(Print_impl)
let _ = Q.Count_by_id.select () ~source:Q.Count_by_id.Source.table_external ~id:7L
