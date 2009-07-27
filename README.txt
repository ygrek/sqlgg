
Build
=====

Dependencies
------------

* menhir
* extlib
* oUnit
* deriving

NB `deriving` is not available in Debian packages

`deriving` conflicts with extLib in native (ocamlopt) build. 
See http://code.google.com/p/deriving/issues/detail?id=1
So you have to alter `deriving/lib/Makefile` and 
remove the `enum.ml enum.mli` line from SOURCES beforehand.

Dependencies will be found with ocamlfind by default.
Alternatively you can create `src/myocamlbuild.config` and manually 
specify paths to libraries as follows :

    extlib=C:/my/contrib/extlib-1.5.1
    deriving=C:/my/contrib/deriving-0.1.1/lib
    oUnit=C:/my/contrib/ounit-1.0.3

sqlgg
-----

cd src && ./build
(or build.bat for Windows)

----
2009-07-27
