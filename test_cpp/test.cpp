//

#include "../sqlite3_helper.hpp"
#include "test.hpp"
#include <boost/foreach.hpp>
#include <iostream>
#include <vector>

using namespace std;

typedef sql2cpp::test<sqlite3_traits> table;
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

int main()
{
  sqlite3* db = NULL;
  int nResult = SQLITE_OK;

  nResult = sqlite3_open("test.db", &db);
  cout << "open : " << nResult << " " << sqlite3_errmsg(db) << endl;

  nResult = sqlite3_exec(db,"DROP TABLE test;",NULL,NULL,NULL);
  cout << "drop : " << nResult << " " << sqlite3_errmsg(db) << endl;

  nResult = table::create(db);
  cout << "create : " << nResult << " " << sqlite3_errmsg(db) << endl;

  table::row t;
  t.name="c++";
  t.descr="ugly";
  nResult = table::Add(db,t);
  cout << "insert : " << nResult << " " << sqlite3_errmsg(db) << endl;

  t.name="c";
  t.descr="hard";
  nResult = table::Add(db,t);
  cout << "insert : " << nResult << " " << sqlite3_errmsg(db) << endl;

  t.name="ocaml";
  t.descr="wonderful";
  nResult = table::Add(db,t);
  cout << "insert : " << nResult << " " << sqlite3_errmsg(db) << endl;

  std::vector<table::row> all;
  nResult = table::select_all(db,all);
  cout << "select : " << nResult << " " << sqlite3_errmsg(db) << endl;

  BOOST_FOREACH(const table::row& q, all)
  {
     std::cout << q.id << ") " << q.name << " is " << q.descr << std::endl;
  }

  nResult = sqlite3_close(db);
  cout << "close : " << nResult << endl;

  return 0;
}
