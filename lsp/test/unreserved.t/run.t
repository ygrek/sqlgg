PostgreSQL unreserved keywords behave as identifiers in IDE features:

  $ ../ask.exe q.sql hover:'SELECT extension^' hover:'extension, version^' hover:'FROM schema^' def:'SELECT extension^' def:'extension, version^' def:'FROM schema^'
  ### hover:SELECT extension^
  2:7-2:16
  ```sql
  schema.extension  Int?
  ```
  
  Declared in `q.sql`
  ### hover:extension, version^
  2:18-2:25
  ```sql
  schema.version  Text?
  ```
  
  Declared in `q.sql`
  ### hover:FROM schema^
  2:31-2:37
  **table** `schema`
  
  ```sql
  extension  Int?
  version    Text?
  ```
  
  Declared in `q.sql`
  ### def:SELECT extension^
  q.sql 1:21-1:30
  ### def:extension, version^
  q.sql 1:40-1:47
  ### def:FROM schema^
  q.sql 1:13-1:19
