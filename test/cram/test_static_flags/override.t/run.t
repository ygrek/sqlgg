Per-statement [dynamic_select=false] beats the global -dynamic-select flag:
get_user comes out in both versions, get_user_classic stays classic only.

  $ cat override.sql | sqlgg -gen caml -no-header -dialect=mysql -dynamic-select - > override.ml
  $ diff override.ml override.compare.ml

An unknown value is an error, no code is generated:

  $ cat override.sql | sed 's/dynamic_select=false/dynamic_select=garbage/' | sqlgg -gen caml -no-header -dialect=mysql -
  unknown dynamic_select=garbage (expected true, both or false) at 110-144
  Errors encountered, no code generated
  [1]

  $ cat override.sql | sed 's/dynamic_select=false/dynamik_select=false/' | sqlgg -gen caml -no-header -dialect=mysql -
  unknown property dynamik_select at 110-142
  Errors encountered, no code generated
  [1]
  $ cat override.sql | sed 's/dynamic_select=false/noparse=maybe/' | sqlgg -gen caml -no-header -dialect=mysql -
  noparse is a flag, it does not take a value at 110-135
  Errors encountered, no code generated
  [1]
