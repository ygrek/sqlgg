Redefining an existing index (changing its column set) is a drop + re-add, not a
no-op: the index `ix` goes from (a) to (a, b). The generator emits a single
ALTER that drops and re-adds the index under the same name, and the down rebuilds
the original (a) definition. Verified internally before printing; the generator
must reproduce the committed golden byte-for-byte (no diff output == equal):

  $ sqlgg -no-header -dialect mysql -diff -now 20260101000000 -gen sql -base initial.sql -target target.sql | diff migrations.sql -
