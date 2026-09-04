The generated dynamic-select parameter has no source token and does not shadow
the first projected column:

  $ ../ask.exe q.sql tokens 'hover:SELECT id^'
  ### tokens
  10:58-10:61 parameter
  ### hover:SELECT id^
  10:7-10:9
  ```sql
  products.id  Int
  ```
  
  Declared in `q.sql`
