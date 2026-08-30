Schema files come from sqlgg.json above the document, globs and explicit paths alike;
what the glob does not match is unknown:

  $ ../ask.exe queries/q.sql diags hover:'posts JOIN' def:'users ON' def:'title FROM' complete:'posts JOIN'
  ### diags
  3:0-3:21 no such table skipped
  ### hover:posts JOIN
  2:18-2:23
  **table** `posts`
  
  ```sql
  id     Int?
  title  Text?
  ```
  
  Declared in `./schema/02_posts.sql`
  ### def:users ON
  ./schema/01_users.sql 1:13-1:18
  ### def:title FROM
  ./schema/02_posts.sql 1:28-1:33
  ### complete:posts JOIN
  replace 2:18-2:23
  posts  table — 2 columns
  extra  table — 1 columns
  users  table — 2 columns
