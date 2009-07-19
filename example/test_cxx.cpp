#include "../impl/sqlite3_traits.hpp"
#include "test_cxx_gen.hpp"
#include <iostream>
#include <vector>

typedef sqlgg<sqlite3_traits> gen_t;

/*
void to_console(const wchar_t* s)
{
  HANDLE console = GetStdHandle(STD_OUTPUT_HANDLE);

  DWORD ignore;
  WriteConsoleW(console, s, wcslen(s), &ignore, NULL);
}

void to_console(const std::wstring& s)
{
   to_console(s.c_str());
}
*/

void explain(char const* msg, sqlite3* db)
{
  std::cout << msg << " : " << sqlite3_errcode(db) << " : " << sqlite3_errmsg(db) << std::endl;
}

struct output
{
  void operator()(int id, std::string name, std::string desc)
  {
    std::cout << id << ") " << name << " is " << desc << std::endl;
  }
};

int main()
{
  sqlite3* db = NULL;

  sqlite3_open(":memory:", &db);
  explain("open",db);

  gen_t gen(db);

/*
  gen.drop_test();
  explain("drop",db);
  gen.drop_loc();
  explain("drop",db);
  gen.drop_zuzu();
  explain("drop",db);
*/

  gen.create_test();
  explain("create",db);

/*
  gen::data_1 t;
  t.name="c++";
  t.descr="ugly";
  nResult = gen::Add(db,t);
  cout << "insert : " << nResult << " " << sqlite3_errmsg(db) << endl;
*/
  gen.Add("c++","ugly");
  explain("insert",db);

  gen.Add("c","hard");
  explain("insert",db);

  gen.Add("ocaml","wonderful");
  explain("insert",db);

  gen.Exaggerate("really");
  explain("update",db);

  gen.select_all(output());
  explain("select",db);

  gen.create_loc();
  explain("create_loc",db);

  gen.create_zuzu("qq");
  explain("create_zuzu",db);

  sqlite3_close(db);

  return 0;
}
