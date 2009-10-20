
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

    make -C src
    (or src\build.bat for Windows)

Windows users
=============

Install VS2005 SP1 redistributable
<http://www.microsoft.com/downloads/details.aspx?FamilyID=200b2fd9-ae1a-4a14-984d-389c36f85647>

----
2009-10-20
