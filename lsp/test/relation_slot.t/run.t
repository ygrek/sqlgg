Relation slots: the alias completes the columns every accepted relation has, the
slot is listed among the parameters, and a slot nothing fits is a diagnostic:

  $ ../ask.exe q.sql diags hover:'@source AS s WHERE' complete:'s.na'
  ### diags
  9:22-9:29 no relation is compatible with relation slot @source
    users: missing attribute : missing
    drafts: missing attribute : missing
    logs: missing attribute : missing
  ### hover:@source AS s WHERE
  6:25-6:32
  ```sql
  @source — relation  
  ```
  ### complete:s.na
  replace 6:13-6:14
  id  Int — s
  name  Text? — s
  s  source — 2 columns
  any_value  function
  avg  function
  coalesce  function
  concat  function
  concat_ws  function
  count  function
  current_date  function
  current_time  function
  current_timestamp  function
