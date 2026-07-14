Per-statement [dynamic_select=false] beats the global -dynamic-select flag:
get_user comes out in both versions, get_user_classic stays classic only.

  $ cat override.sql | sqlgg -gen caml -no-header -dialect=mysql -dynamic-select - > override.ml
  $ diff override.ml override.compare.ml

An unknown value is an error, no code is generated:

  $ cat override.sql | sed 's/dynamic_select=false/dynamic_select=garbage/' | sqlgg -gen caml -no-header -dialect=mysql -
  Unknown dynamic_select=garbage (expected true, both or false)
  Errors encountered, no code generated
  [1]
