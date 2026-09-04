Diagnostics and parameters:

  $ ../ask.exe q.sql diags tokens
  ### diags
  17:11-17:15 missing attribute : nmae
  20:9-20:12 syntax error
  25:17-25:22 missing attribute : nmae2
  ### tokens
  8:45-8:48 parameter
  23:27-23:34 enum
  23:37-23:43 enumMember
  23:53-23:55 parameter
  23:60-23:64 enumMember
  23:72-23:74 parameter
  23:79-23:82 enumMember
  31:33-31:37 parameter
  31:54-31:63 parameter

Hover: statement, table, column, parameter, expressions (also inside ORDER BY of an aggregate), choice and its branches:

  $ ../ask.exe q.sql hover:'SELECT id, name' hover:'users WHERE' hover:'email FROM' hover:@id hover:'count(*)' hover:'+ 1' hover:@filter hover:ByName hover:'= @n' hover:'TRUE }'
  ### hover:SELECT id, name
  8:0-8:48
  `get_user` — SELECT — at most one row
  
  **Parameters**
  
  ```sql
  @id  Int
  ```
  
  **Result**
  
  ```sql
  id     Int
  name   Text
  email  Text?
  ```
  ### hover:users WHERE
  8:28-8:33
  **table** `users`
  
  ```sql
  id     Int
  name   Text
  email  Text?
  ```
  
  Declared in `q.sql`
  ### hover:email FROM
  8:17-8:22
  ```sql
  users.email  Text?
  ```
  
  Declared in `q.sql`
  ### hover:@id
  8:45-8:48
  ```sql
  @id  Int
  ```
  ### hover:count(*)
  11:7-11:15
  ```sql
  expression  Int
  ```
  ### hover:+ 1
  14:34-14:40
  ```sql
  expression  Int
  ```
  ### hover:@filter
  23:27-23:34
  ```sql
  @filter  
  ```
  
  **Branches**
  
  ```sql
  ByName  
    @n    Text
  ById    
    @i    Int
  All     
  ```
  ### hover:ByName
  23:37-23:56
  branch `ByName` of `@filter`
  
  **Parameters in this branch**
  
  ```sql
  @n  Text
  ```
  ### hover:= @n
  23:37-23:56
  branch `ByName` of `@filter`
  
  **Parameters in this branch**
  
  ```sql
  @n  Text
  ```
  ### hover:TRUE }
  23:79-23:90
  branch `All` of `@filter`
  
  Takes no parameters.

Definition of a table and of a column, and nothing for a parameter:

  $ ../ask.exe q.sql def:'users WHERE' def:'email FROM' def:@id
  ### def:users WHERE
  q.sql 1:13-1:18
  ### def:email FROM
  q.sql 4:2-4:7
  ### def:@id
  nothing

Text that is not code:

  $ ../ask.exe q.sql hover:'mentioning users' hover:"'users'" complete:'mentioning users' complete:"users' AS" 
  ### hover:mentioning users
  nothing
  ### hover:'users'
  28:0-28:30
  SELECT — any number of rows
  
  **Result**
  
  ```sql
  s  StringLiteral (users)
  ```
  ### complete:mentioning users
  replace 27:13-27:13
  ### complete:users' AS
  replace 28:8-28:8

Completion: in an expression, at a table position, of a parameter, and next to a qualified column
where the qualifier must not be taken for an alias:

  $ ../ask.exe q.sql complete:nmae complete:FRM complete:'users;' complete:'@i^' complete:nmae2
  ### complete:nmae
  replace 17:11-17:15
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
  ### complete:FRM
  replace 20:9-20:12
  EXCEPT  keyword
  FOR  keyword
  FROM  keyword
  GROUP  keyword
  HAVING  keyword
  INTERSECT  keyword
  LIMIT  keyword
  LOCK  keyword
  ORDER  keyword
  UNION  keyword
  WHERE  keyword
  ### complete:users;
  replace 11:30-11:35
  users  table — 3 columns
  ### complete:@i^
  replace 8:45-8:48
  @excluded  parameter
  @filter  parameter
  @i  parameter
  @ids  parameter
  @n  parameter
  ### complete:nmae2
  replace 25:17-25:22
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

A list parameter is one row typed after its element, not a choice with the element for a branch:

  $ ../ask.exe q.sql hover:@ids hover:@excluded hover:'SELECT id FROM users WHERE id IN'
  ### hover:@ids
  31:33-31:37
  ```sql
  @ids — IN  Int list
  ```
  ### hover:@excluded
  31:54-31:63
  ```sql
  @excluded — NOT IN  Text list
  ```
  ### hover:SELECT id FROM users WHERE id IN
  31:0-31:63
  `in_list` — SELECT — any number of rows
  
  **Parameters**
  
  ```sql
  @ids — IN           Int list
  @excluded — NOT IN  Text list
  ```
  
  **Result**
  
  ```sql
  id  Int
  ```
