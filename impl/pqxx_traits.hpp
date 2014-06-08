/*
  Postgresql/pqxx C++ traits for sqlgg
  by ygrek
  2014-06-08

  This is free and unencumbered software released into the public domain.

  Anyone is free to copy, modify, publish, use, compile, sell, or
  distribute this software, either in source code form or as a compiled
  binary, for any purpose, commercial or non-commercial, and by any
  means.

  For more information, please refer to <http://unlicense.org/>
*/

#include <pqxx/pqxx>
#define SQLGG_STR(x) x

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include <string>
#include <vector>

#if defined(SQLGG_DEBUG)
#include <iostream>
using namespace std;
#endif

struct pqxx_traits
{
  typedef int Int;
  typedef std::string Text;
  typedef Text Any;

  typedef pqxx::result::const_iterator row;
  typedef pqxx::work& connection;

  static void get_column(row r, int index, Int& data)
  {
    // nothing
  }

  static void get_column(row r, int index, Text& data)
  {
  }

  typedef pqxx::prepare::declaration stmt_decl;

  static void set_param(stmt_decl const& stmt, const Text& val, int index)
  {
    stmt("varchar",pqxx::prepare::treat_string);
  }

  static void set_param(stmt_decl const& stmt, const Int& val, int index)
  {
    stmt("INTEGER",pqxx::prepare::treat_direct);
  }

  template<typename T>
  static void set_param(pqxx::prepare::invocation& stmt, const T& val, int index)
  {
    stmt(val);
  }

  template<class Container, class Binder, class Params>
  static bool do_select(connection db, Container& result, const char* sql, Binder binder, Params params)
  {
    const char* name = "sqlgg_stmt";
    cout << "start prepare" << endl;
    pqxx::prepare::declaration decl = db.conn().prepare(name,sql);
    params.set_params(decl);

    cout << "start invoke" << endl;
    pqxx::prepare::invocation call = db.prepared(name);
    cout << "set params" << endl;
    params.set_params(call);
    cout << "exec" << endl;
    result = call.exec();
    cout << "execed" << endl;

    db.conn().unprepare(name);

    return true;
  }

  struct no_params
  {
    void set_params(pqxx::prepare::declaration const&) {}
    void set_params(pqxx::prepare::invocation&) {}
    enum { count = 0 };
  };

  template<class T>
  struct no_binder
  {
    void get(row,T&) {}
    void bind(row,T&) {}
    enum { count = 0 };
  };

  template<class Params>
  static bool do_execute(connection db, const char* sql, Params params)
  {
    pqxx::result R;
    return do_select(db,R,sql,no_binder<int>(),params);
  }

}; // mysql_traits

