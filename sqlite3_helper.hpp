//

#include "sqlite3.h"
//#include <tchar.h>
//#include <atlbase.h>
#define ATLASSERT assert
#if defined(_UNICODE)
#define TCHAR wchar_t
#define _T(x) L##x
#else
#define TCHAR char
#define _T(x) x
#endif
#include <assert.h>
#include <string>

struct sqlite3_traits
{

  typedef int Int;
  typedef std::basic_string<TCHAR> Text;
  typedef Text Any;

  template<class T>
  static void get_column_Int(sqlite3_stmt* stmt, int index, T& val)
  {
     val = sqlite3_column_int(stmt, index);
  }

  template<class T>
  static void get_column_Text(sqlite3_stmt* stmt, int index, T& val)
  {
  #if defined(_UNICODE) || defined(UNICODE)
     val = (const TCHAR*)sqlite3_column_text16(stmt, index);
  #else   
     val = (const TCHAR*)sqlite3_column_text(stmt, index);
  #endif   
  }

  static void get_param_null(sqlite3_stmt* stmt, int index)
  {
      int nResult = sqlite3_bind_null(stmt, index);
      ATLASSERT(SQLITE_OK == nResult);
  }

  static void set_param_Text(sqlite3_stmt* stmt, const Text& val, int index)
  {
  #if defined(_UNICODE) || defined(UNICODE)
      int nResult = sqlite3_bind_text16(stmt, index, val.c_str(), -1, SQLITE_TRANSIENT);
  #else
      int nResult = sqlite3_bind_text(stmt, index, val.c_str(), -1, SQLITE_TRANSIENT);
  #endif     
      ATLASSERT(SQLITE_OK == nResult);
  }

  static void set_param_Int(sqlite3_stmt* stmt, const Int& val, int index)
  {
      int nResult = sqlite3_bind_int(stmt, index, val);
      ATLASSERT(SQLITE_OK == nResult);
  }

  template<class Container, class Binder, class Params>
  static bool do_select(sqlite3* db, Container& result, const TCHAR* sql, Binder binder, Params params)
  {
      sqlite3_stmt *stmt;
      const TCHAR *pszTail;
      int nResult;
  #if defined(_UNICODE) || defined(UNICODE)
      nResult = sqlite3_prepare16(db, sql, -1, &stmt, (void const**)&pszTail);
  #else
      nResult = sqlite3_prepare(db, sql, -1, &stmt, &pszTail);
  #endif
      ATLASSERT(SQLITE_OK == nResult);
      //printf("%u\n%S\n%s\n",nResult,sql,sqlite3_errmsg(db));
      if (SQLITE_OK != nResult)
      {
          //log_sqlite(sql,instDB);
          return false;
      }

      params.set_params(stmt);

      result.clear();

      while(SQLITE_ROW == sqlite3_step(stmt)) //Iterate all objects
      {
          result.push_back(typename Container::value_type());
          binder.of_stmt(stmt,result.back());
      }

      //Destroy the command
      nResult = sqlite3_finalize(stmt);
      if (SQLITE_OK != nResult)
      {
          return false;
      }

      return true;
  }

  template<class Binder>
  static bool do_insert(sqlite3* db, const typename Binder::value_type& val, const TCHAR* sql)
  {
      sqlite3_stmt *stmt;
      int nResult;
      const TCHAR *pszTail;
  #if defined(_UNICODE) || defined(UNICODE)
      nResult = sqlite3_prepare16(db, sql, -1, &stmt, (void const**)&pszTail);
  #else
      nResult = sqlite3_prepare(db, sql, -1, &stmt, &pszTail);
  #endif
      ATLASSERT(SQLITE_OK == nResult);
      if (SQLITE_OK != nResult)
      {
          return false;
      }

      Binder::to_stmt(stmt,val);
      //Execute the command
      nResult = sqlite3_step(stmt);
      ATLASSERT(SQLITE_DONE == nResult);

      //Destroy the command
      nResult = sqlite3_finalize(stmt);
      if (SQLITE_OK != nResult)
      {
          return false;
      }

      return true;
  }

  struct no_params
  {
    void set_params(sqlite3_stmt*) {}
  };

  template<class Params>
  static int do_execute(sqlite3* db, const char* sql, Params params)
  {
      sqlite3_stmt *stmt;
      int nResult;
      const char *pszTail;
      nResult = sqlite3_prepare(db, sql, -1, &stmt, &pszTail);
      //ATLASSERT(SQLITE_OK == nResult);
      if (SQLITE_OK != nResult)
      {
          return nResult;
      }

      params.set_params(stmt);

      //Execute the command
      nResult = sqlite3_step(stmt);
      ATLASSERT(SQLITE_DONE == nResult);

      //Destroy the command
      nResult = sqlite3_finalize(stmt);

      return nResult;
  }

}; // sqlite3_traits
