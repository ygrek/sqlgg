With dynamic_select=both a statement yields the dynamic query and a static twin
named with the _static suffix. A statement that is also reusable yields no twin:

  $ cat > both.sql <<'EOF'
  > CREATE TABLE users (id INT NOT NULL, name TEXT NOT NULL);
  > -- [sqlgg] dynamic_select=both
  > -- @get_user | include: reuse_and_execute
  > SELECT id, name FROM users WHERE id = @id;
  > -- [sqlgg] dynamic_select=both
  > -- @find_user
  > SELECT id, name FROM users WHERE id = @id;
  > EOF
  $ sqlgg -no-header -gen xml both.sql
  <?xml version="1.0"?>
  
  <sqlgg>
   <stmt name="create_users" sql="CREATE TABLE users (id INT NOT NULL, name TEXT NOT NULL)" category="DDL" kind="create" target="users" cardinality="0">
    <in/>
    <out/>
   </stmt>
   <stmt name="get_user" sql="SELECT {TODO dynamic choice} FROM users WHERE id = @id" category="DQL" kind="select" cardinality="n">
    <in>
     <value name="id" type="Int"/>
    </in>
    <out/>
   </stmt>
   <stmt name="find_user" sql="SELECT {TODO dynamic choice} FROM users WHERE id = @id" category="DQL" kind="select" cardinality="n">
    <in>
     <value name="id" type="Int"/>
    </in>
    <out/>
   </stmt>
   <stmt name="find_user_static" sql="SELECT id, name FROM users WHERE id = @id" category="DQL" kind="select" cardinality="n">
    <in>
     <value name="id" type="Int"/>
    </in>
    <out>
     <value name="id" type="Int"/>
     <value name="name" type="Text"/>
    </out>
   </stmt>
   <table name="users">
    <schema>
     <value name="id" type="Int"/>
     <value name="name" type="Text"/>
    </schema>
   </table>
  </sqlgg>

The twin records that dynamic select is off for it:

  $ cat > twin.sql <<'EOF'
  > CREATE TABLE users (id INT NOT NULL, name TEXT NOT NULL);
  > -- [sqlgg] dynamic_select=both
  > -- @find_user
  > SELECT id, name FROM users WHERE id = @id;
  > EOF
  $ sqlgg -no-header -gen json twin.sql
  {"sqlgg_version":null,"module_name":"sqlgg","dialect":"MySQL","params":null,"queries":[{"name":"create_users","stmt":{"sql":"CREATE TABLE users (id INT NOT NULL, name TEXT NOT NULL)","schema":[],"vars":[],"kind":["Create",{"db":null,"tn":"users"}],"props":[["File","twin.sql"]]},"template":[["Text","CREATE TABLE users (id INT NOT NULL, name TEXT NOT NULL)"]]},{"name":"find_user","stmt":{"sql":"SELECT id, name FROM users WHERE id = @id","schema":[["Dynamic",{"value":"col","pos":[7,15]},[{"field_id":{"value":"id","pos":[7,9]},"field_attr":{"name":"","domain":{"t":"Int","nullability":"Strict"},"extra":[],"meta":{}},"join_deps":[]},{"field_id":{"value":"name","pos":[11,15]},"field_attr":{"name":"","domain":{"t":"Text","nullability":"Strict"},"extra":[],"meta":{}},"join_deps":[]}]]],"vars":[["DynamicSelect",{"value":"col","pos":[7,15]},[["Simple",{"ctor":{"value":"id","pos":[7,9]},"ctor_pos":[0,0],"body":[]}],["Simple",{"ctor":{"value":"name","pos":[11,15]},"ctor_pos":[0,0],"body":[]}]]],["Single",{"id":{"value":"id","pos":[38,41]},"typ":{"t":"Int","nullability":"Strict"}},{}]],"kind":["Select","Nat"],"props":[["File","twin.sql"],["Name","find_user"],["Dynamic_select","Both"]]},"template":[["Text","SELECT "],["DynamicSelect",{"value":"col","pos":[7,15]},[{"ctor":{"value":"id","pos":[7,9]},"args":[],"sql":[["Text","id"]]},{"ctor":{"value":"name","pos":[11,15]},"args":[],"sql":[["Text","name"]]}]],["Text"," FROM users WHERE id = "],["Bind",{"param":{"id":{"value":"id","pos":[38,41]},"typ":{"t":"Int","nullability":"Strict"}},"original":"@id"}]]},{"name":"find_user_static","stmt":{"sql":"SELECT id, name FROM users WHERE id = @id","schema":[["Attr",{"name":"id","domain":{"t":"Int","nullability":"Strict"},"extra":["NotNull"],"meta":{}}],["Attr",{"name":"name","domain":{"t":"Text","nullability":"Strict"},"extra":["NotNull"],"meta":{}}]],"vars":[["Single",{"id":{"value":"id","pos":[38,41]},"typ":{"t":"Int","nullability":"Strict"}},{}]],"kind":["Select","Nat"],"props":[["File","twin.sql"],["Dynamic_select","Off"],["Name","find_user_static"]]},"template":[["Text","SELECT id, name FROM users WHERE id = "],["Bind",{"param":{"id":{"value":"id","pos":[38,41]},"typ":{"t":"Int","nullability":"Strict"}},"original":"@id"}]]}],"tables":[{"name":{"db":null,"tn":"users"},"columns":[{"attr":{"name":"id","domain":{"t":"Int","nullability":"Strict"},"extra":["NotNull"],"meta":{}},"source_kind":{"collated":["Int",{"size":null,"sign":"Signed","display_width":null}],"collation":null},"default_sql":null},{"attr":{"name":"name","domain":{"t":"Text","nullability":"Strict"},"extra":["NotNull"],"meta":{}},"source_kind":{"collated":["Text",["PlainText",null]],"collation":null},"default_sql":null}],"tbl_charset":null,"tbl_ttl":null,"tbl_indexes":{},"tbl_foreign_keys":[]}]}

A dialect feature of such a statement is reported once, not once per query:

  $ sqlgg -no-header -dialect sqlite -gen caml - <<'EOF'
  > CREATE TABLE users (id INT NOT NULL, name TEXT NOT NULL);
  > -- [sqlgg] dynamic_select=both
  > -- @get_user
  > SELECT id, name FROM users WHERE id = @id FOR UPDATE;
  > EOF
  Feature RowLocking is not supported for dialect SQLite (supported by: PostgreSQL, MySQL, TiDB) at FOR UPDATE
  Errors encountered, no code generated
  [1]
