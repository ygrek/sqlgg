del sql2cpp.exe
git rev-parse HEAD | ocaml make_version.ml > version.ml
ocamlbuild -classic-display -no-links -cflags -dtypes,-w,Az sql2cpp.byte
copy _build\sql2cpp.byte sql2cpp.exe