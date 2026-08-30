Completion checks the statement against the schema as it was *before* that
statement, not after the whole document: the first SELECT still sees `name`
even though `t` is dropped and recreated without it further down.

  $ ../ask.exe q.sql diags complete:'na FROM t WHERE' complete:'na FROM t;'
  ### diags
  3:7-3:9 missing attribute : na
  9:7-9:9 missing attribute : na
  ### complete:na FROM t WHERE
  replace 3:7-3:9
  id  Int? — t
  name  Text? — t
  t  source — 2 columns
  any_value  function
  avg  function
  coalesce  function
  concat  function
  concat_ws  function
  count  function
  current_date  function
  current_time  function
  current_timestamp  function
  ### complete:na FROM t;
  replace 9:7-9:9
  id  Int? — t
  t  source — 1 columns
  any_value  function
  avg  function
  coalesce  function
  concat  function
  concat_ws  function
  count  function
  current_date  function
  current_time  function
  current_timestamp  function
  date  function
