Aliases, CTEs and subqueries in a statement that type-checks; a CTE column leads to its expression:

  $ ../ask.exe q.sql diags hover:'u.name' hover:'r.title' hover:'sub.n' hover:'recent AS r' def:'u.name' def:'r.title' def:'title, sub' def:'recent AS r' def:'sub.n' complete:'sub.n'
  ### diags
  11:7-11:13 missing attribute : nmae
  13:23-13:27 missing attribute : nmae
  18:4-18:11 missing attribute : nmae
  23:7-23:12 missing attribute : title
  24:7-24:9 duplicate attribute : id
  ### hover:u.name
  5:7-5:8
  **table** `users`
  
  ```sql
  id    Int?
  name  Text?
  ```
  
  Declared in `q.sql`
  ### hover:r.title
  5:15-5:16
  `r`
  
  ```sql
  id     Int
  title  Text?
  ```
  
  Available in this statement
  ### hover:sub.n
  5:24-5:27
  `sub`
  
  ```sql
  author  Int?
  n       Int
  ```
  
  Available in this statement
  ### hover:recent AS r
  7:5-7:11
  **CTE** `recent`
  
  ```sql
  id     Int
  title  Text?
  ```
  
  Available in this statement
  ### def:u.name
  q.sql 1:13-1:18
  ### def:r.title
  q.sql 7:15-7:16
  ### def:title, sub
  q.sql 4:27-4:32
  ### def:recent AS r
  q.sql 4:5-4:11
  ### def:sub.n
  q.sql 8:66-8:69
  ### complete:sub.n
  replace 5:24-5:27
  author  Int? — sub
  id  Int? — users
  n  Int — sub
  name  Text? — users
  title  Text? — recent
  r  source — 2 columns
  recent  source — 2 columns
  sub  source — 2 columns
  u  source — 2 columns
  users  source — 2 columns
  any_value  function
  avg  function

The same in statements that do not type-check, thanks to the fallback on the sources alone:

  $ ../ask.exe q.sql hover:'r.id FROM' def:'recent AS r' complete:nmae hover:'uu.id' complete:'nmae ='
  ### hover:r.id FROM
  11:15-11:16
  `r`
  
  ```sql
  id  Int?
  ```
  
  Available in this statement
  ### def:recent AS r
  q.sql 4:5-4:11
  ### complete:nmae
  replace 11:9-11:13
  id  Int? — u
  name  Text? — u
  ### hover:uu.id
  13:40-13:42
  **table** `users`
  
  ```sql
  id    Int?
  name  Text?
  ```
  
  Declared in `q.sql`
  ### complete:nmae =
  replace 13:23-13:27
  id  Int? — users
  name  Text? — users
  users  source — 2 columns
  uu  source — 2 columns

A multi-table UPDATE that does not type-check : every source of the table list stays in
scope, not just the first one.

  $ ../ask.exe q.sql hover:'agg.n WHERE' def:'agg.n WHERE' complete:'n WHERE'
  ### hover:agg.n WHERE
  18:14-18:17
  `agg`
  
  ```sql
  author  Int?
  n       Int
  ```
  
  Available in this statement
  ### def:agg.n WHERE
  q.sql 17:81-17:84
  ### complete:n WHERE
  replace 18:18-18:19
  author  Int? — agg
  n  Int — agg

A subquery inside WHERE:

  $ ../ask.exe q.sql hover:'p.title' def:'p.title'
  ### hover:p.title
  15:70-15:71
  **table** `posts`
  
  ```sql
  id      Int?
  author  Int?
  title   Text?
  ```
  
  Declared in `q.sql`
  ### def:p.title
  q.sql 2:13-2:18

Unqualified columns are limited to the current statement scope, including
aliases, missing columns, and ambiguity:

  $ ../ask.exe q.sql hover:'id FROM users;' hover:'id FROM users AS only_users' hover:'title FROM users;' hover:'id FROM users JOIN' | grep -E '^###|^[a-z_]+\.id|^nothing$'
  ### hover:id FROM users;
  users.id  Int?
  ### hover:id FROM users AS only_users
  users.id  Int?
  ### hover:title FROM users;
  nothing
  ### hover:id FROM users JOIN
  users.id  Int?
  posts.id  Int?

Nested SELECT scopes do not leak into each other:

  $ ../ask.exe q.sql hover:'id FROM users WHERE' hover:'author FROM posts AS p' | grep -E '^###|^[a-z_]+\.[a-z_]+  '
  ### hover:id FROM users WHERE
  users.id  Int?
  ### hover:author FROM posts AS p
  posts.author  Int?

  $ ../ask.exe q.sql complete:'id FROM users WHERE' complete:'author FROM posts AS p' | grep -E '^###|^(author|id|name|title)  '
  ### complete:id FROM users WHERE
  id  Int? — users
  name  Text? — users
  ### complete:author FROM posts AS p
  author  Int? — posts
  id  Int? — posts
  title  Text? — posts

CTE bodies, FROM subqueries, and ORDER BY use their own SELECT scope:

  $ ../ask.exe q.sql hover:'id, title FROM posts' hover:'author, count' hover:'ORDER BY id^' | grep -E '^###|^[a-z_]+\.[a-z_]+  '
  ### hover:id, title FROM posts
  posts.id  Int?
  ### hover:author, count
  posts.author  Int?
  ### hover:ORDER BY id^
  users.id  Int?

  $ ../ask.exe q.sql complete:'id, title FROM posts' complete:'author, count' complete:'ORDER BY id^' | grep -E '^###|^(author|id|name|title)  '
  ### complete:id, title FROM posts
  author  Int? — posts
  id  Int? — posts
  title  Text? — posts
  ### complete:author, count
  author  Int? — posts
  id  Int? — posts
  title  Text? — posts
  ### complete:ORDER BY id^
  id  Int? — users
  name  Text? — users

Empty and compound SELECTs keep independent scopes:

  $ ../ask.exe q.sql hover:'id FROM users UNION' hover:'id FROM posts;' | grep -E '^###|^[a-z_]+\.[a-z_]+  '
  ### hover:id FROM users UNION
  users.id  Int?
  ### hover:id FROM posts;
  posts.id  Int?

  $ ../ask.exe q.sql complete:'1 AS one' complete:'id FROM users UNION' complete:'id FROM posts;' | grep -E '^###|^(author|id|name|title)  '
  ### complete:1 AS one
  ### complete:id FROM users UNION
  id  Int? — users
  name  Text? — users
  ### complete:id FROM posts;
  author  Int? — posts
  id  Int? — posts
  title  Text? — posts

INSERT, UPDATE, and DELETE use their target table as scope:

  $ ../ask.exe q.sql hover:'id, name) VALUES' hover:'name = @next_name' hover:'id = @delete_id' | grep -E '^###|^[a-z_]+\.[a-z_]+  '
  ### hover:id, name) VALUES
  users.id  Int?
  ### hover:name = @next_name
  users.name  Text?
  ### hover:id = @delete_id
  users.id  Int?

  $ ../ask.exe q.sql complete:'id, name) VALUES' complete:'name = @next_name' complete:'id = @delete_id' | grep -E '^###|^(id|name)  '
  ### complete:id, name) VALUES
  id  Int? — users
  name  Text? — users
  ### complete:name = @next_name
  id  Int? — users
  name  Text? — users
  ### complete:id = @delete_id
  id  Int? — users
  name  Text? — users

Reusable query references link to their declaration; annotation comments stay
outside statement hover:

  $ ../ask.exe q.sql hover:@shared_users 'hover:&shared_users' 'hover:name FROM users;' 'def:&shared_users' | grep -E '^###|^nothing$|^`shared_users`|^[a-z_]+\.[a-z_]+  |^q.sql'
  ### hover:@shared_users
  nothing
  ### hover:&shared_users
  `shared_users` — SELECT — any number of rows
  ### hover:name FROM users;
  users.name  Text?
  ### def:&shared_users
  q.sql 32:0-32:22
