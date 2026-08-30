Completion after a syntax error earlier in the statement : the automaton recovers by
unwinding states and stepping over tokens, and the cursor still gets what the grammar
expects there, with the tables of the statement as sources.

  $ ../ask.exe q.sql complete:na1 complete:na2 complete:na3 complete:'ti;'
  ### complete:na1
  replace 14:28-14:31
  email  Text? — users
  id  Int — users
  name  Text — users
  users  source — 3 columns
  any_value  function
  avg  function
  coalesce  function
  concat  function
  concat_ws  function
  count  function
  current_date  function
  current_time  function
  ### complete:na2
  replace 17:36-17:39
  email  Text? — users
  id  Int — users
  name  Text — users
  users  source — 3 columns
  any_value  function
  avg  function
  coalesce  function
  concat  function
  concat_ws  function
  count  function
  current_date  function
  current_time  function
  ### complete:na3
  replace 20:25-20:28
  email  Text? — users
  id  Int — users
  name  Text — users
  users  source — 3 columns
  any_value  function
  avg  function
  coalesce  function
  concat  function
  concat_ws  function
  count  function
  current_date  function
  current_time  function
  ### complete:ti;
  replace 23:63-23:65
  author_id  Int — p
  id  Int — p
  title  Text? — p
