
git rev-parse HEAD | ocaml make_version.ml > version.ml
ocamlbuild -classic-display -no-links -cflags -dtypes,-w,Az sql2cpp.byte
