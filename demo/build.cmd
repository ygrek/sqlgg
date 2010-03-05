
../sqlgg -gen cxx demo_sqlite3.sql > demo_cxx_gen.hpp
../sqlgg -gen cxx -params unnamed demo_mysql.sql > demo_cxx_gen_mysql.hpp
../sqlgg -gen caml demo_sqlite3.sql > demo_caml_gen.ml
../sqlgg -gen xml demo_sqlite3.sql > demo_xml_gen.xml
../sqlgg -gen java -name demo_java_gen demo_mysql.sql > demo_java_gen.java
../sqlgg -gen csharp demo_mysql.sql > demo_csharp_gen.cs

cl /c /MDd /D_UNICODE /D_DEBUG /W4 /Zc:forScope,wchar_t /EHsc demo_cxx.cpp
link /DEBUG demo_cxx.obj sqlite3.lib

ocamlfind ocamlc -linkpkg -package sqlite3 -w Alse -g -o demo_caml -I ../impl ../impl/sqlgg_sqlite3.ml ../impl/sqlgg_traits.ml demo_caml_gen.ml demo_caml.ml

cl /c /MDd /D_UNICODE /D_DEBUG /W4 /Zc:forScope,wchar_t /EHsc demo_cxx_mysql.cpp
link /DEBUG demo_cxx_mysql.obj libmysqlclient.lib

javac demo_java.java

csc -r:System.Data -r:MySql.Data demo_csharp.cs demo_csharp_gen.cs

