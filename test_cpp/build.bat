..\sqlgg.byte.exe ..\test.sql > test.hpp
cl /c /MDd /D_UNICODE /D_DEBUG /W4 /Zc:forScope,wchar_t /EHsc test.cpp
link /DEBUG test.obj sqlite3.lib
