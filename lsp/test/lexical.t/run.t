Lexical errors are diagnostics on the offending span, not a dead file : an unterminated
literal ends its statement so the statements after it are still analyzed, an unterminated
comment runs to the end of the file.

  $ ../ask.exe q.sql diags hover:@x
  ### diags
  3:26-3:32 unterminated string literal
  7:30-9:0 unterminated comment
  ### hover:@x
  7:26-7:28
  ```sql
  @x  Int
  ```

A malformed annotation is reported where the property list stops making sense.

  $ ../ask.exe props.sql diags
  ### diags
  2:0-3:0 malformed property list
  4:0-5:0 unknown property dynamik_select
  6:0-7:0 unknown include=sometimes (expected reuse, execute or reuse_and_execute)
