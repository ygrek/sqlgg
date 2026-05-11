The declarative diff is dialect-aware on the input side: the same add-column
delta is produced when parsing under the PostgreSQL dialect. (Generated DDL is
emitted with sqlgg's canonical quoting regardless of dialect.) The generator
must reproduce the committed golden byte-for-byte (no diff output == equal):

  $ sqlgg -no-header -dialect postgresql -diff -now 20260101000000 -gen sql -base initial.sql -target target.sql | diff migrations.sql -
