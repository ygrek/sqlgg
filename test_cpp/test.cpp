//

#include "../sqlite3_helper.hpp"
#include "test.hpp"
#include <boost/foreach.hpp>
#include <iostream>
#include <vector>

using namespace std;

typedef sqlgg<sqlite3_traits> gen;

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
  cout << msg << " : " /*<< sqlite3_errcode(db) << " "*/ << sqlite3_errmsg(db) << endl;
}

int main()
{
  sqlite3* db = NULL;
  int nResult = SQLITE_OK;

  nResult = sqlite3_open("test.db", &db);
  explain("open",db);

  nResult = sqlite3_exec(db,"DROP TABLE test;",NULL,NULL,NULL);
  explain("drop",db);

  nResult = gen::create(db);
  explain("create",db);

/*
  gen::data_1 t;
  t.name="c++";
  t.descr="ugly";
  nResult = gen::Add(db,t);
  cout << "insert : " << nResult << " " << sqlite3_errmsg(db) << endl;
*/
  nResult = gen::Add(db,"c++","ugly");
  explain("insert",db);

  nResult = gen::Add(db,"c","hard");
  explain("insert",db);

  nResult = gen::Add(db,"ocaml","wonderful");
  explain("insert",db);

  nResult = gen::Exaggerate(db,"really");
  explain("update",db);

  std::vector<gen::data_1> all;
  nResult = gen::select_all(db,all);
  explain("select",db);

  BOOST_FOREACH(gen::data_1 const& q, all)
  {
     std::cout << q.id << ") " << q.name << " is " << q.descr << std::endl;
  }

  gen::create_loc(db);
  explain("create_loc",db);

  //nResult = gen::create_zuzu(db,"qq");
  //explain("create_zuzu",db);

  nResult = sqlite3_close(db);
  cout << "close : " << nResult << endl;

  return 0;
}
