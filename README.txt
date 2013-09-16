sqlgg: SQL Guided (code) Generator

Homepage at http://ygrek.org.ua/p/sqlgg/

Build
=====

Dependencies
------------

* menhir
* extlib
* oUnit
* deriving
* ocamlfind (optional)

`deriving` is special - there are several forks floating around.

* Patched version available at <http://repo.or.cz/w/deriving.git> or
  deriving-ocsigen are recommended - everything should work out of the box.

* Otherwise, get the original version from <http://code.google.com/p/deriving>.
  It will cause linking conflicts with `extLib` (see 
  <http://code.google.com/p/deriving/issues/detail?id=1>).
  So you have to edit `deriving/lib/Makefile` (remove the `enum.ml enum.mli` 
  line from SOURCES variable) and rebuild. Mind that it won't
  [build cleanly on Windows](http://code.google.com/p/deriving/issues/detail?id=3).
  Also it doesn't install via ocamlfind and only provides a standalone
  preprocessor. So make sure ocamlbuild will find `deriving.cma` and edit
  `src/_tags` to invoke preprocessor directly - replace flags `camlp4o, pa_deriving`
  with `pp(path/to/deriving/preprocessor)`.

Dependencies will be found with ocamlfind by default.
Additionaly it is possible to specify paths to dependencies in `src/myocamlbuild.config`,
e.g.:

    extlib=C:/my/contrib/extlib-1.5.1
    deriving=C:/my/contrib/deriving-0.1.1/lib
    oUnit=C:/my/contrib/ounit-1.0.3

Build
-----

Change to `src` directory and run `make` (or `build.bat` on Windows).

Windows users
=============

Install VS2005 SP1 redistributable
<http://www.microsoft.com/downloads/details.aspx?FamilyID=200b2fd9-ae1a-4a14-984d-389c36f85647>

----
2013-09-16
