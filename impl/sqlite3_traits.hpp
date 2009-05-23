//

#include <sqlite3.h>
//#include <tchar.h>
//#include <atlbase.h>
#define ATLASSERT assert
#if defined(_UNICODE)
#define TCHAR wchar_t
#define SQLGG_STR(x) L##x
#else
#define TCHAR char
#define SQLGG_STR(x) x
#endif
#include <assert.h>
#include <string>

struct sqlite3_traits
{
  typedef int Int;
  typedef std::basic_string<TCHAR> Text;
  typedef Text Any;

  typedef sqlite3_stmt* row;
  typedef sqlite3* connection;

  static void get_column(row r, int index, Int& val)
  {
     val = sqlite3_column_int(r, index);
  }

  static void get_column(row r, int index, Text& val)
  {
  #if defined(_UNICODE) || defined(UNICODE)
     val = (const TCHAR*)sqlite3_column_text16(r, index);
  #else
     val = (const TCHAR*)sqlite3_column_text(r, index);
  #endif
  }

  static void set_param_Text(row r, const Text& val, int index)
  {
  #if defined(_UNICODE) || defined(UNICODE)
      int nResult = sqlite3_bind_text16(r, index + 1, val.c_str(), -1, SQLITE_TRANSIENT);
  #else
      int nResult = sqlite3_bind_text(r, index + 1, val.c_str(), -1, SQLITE_TRANSIENT);
  #endif
      ATLASSERT(SQLITE_OK == nResult);
  }

  static void set_param_Any(row r, const Any& val, int index)
  {
    set_param_Text(r,val,index);
  }

  static void set_param_Int(row r, const Int& val, int index)
  {
      int nResult = sqlite3_bind_int(r, index + 1, val);
      ATLASSERT(SQLITE_OK == nResult);
  }

  template<class Container, class Binder, class Params>
  static bool do_select(connection db, Container& result, const TCHAR* sql, Binder binder, Params params)
  {
      sqlite3_stmt *stmt;
      const TCHAR *pszTail;
      int nResult;
  #if defined(_UNICODE) || defined(UNICODE)
      nResult = sqlite3_prepare16(db, sql, -1, &stmt, (void const**)&pszTail);
  #else
      nResult = sqlite3_prepare(db, sql, -1, &stmt, &pszTail);
  #endif

      if (SQLITE_OK != nResult)
      {
#if defined(SQLGG_DEBUG)
        printf("sqlite3_prepare error (%u):%s\n%s\n",nResult,sqlite3_errmsg(db),sql);
#endif
        //log_sqlite(sql,instDB);
        ATLASSERT(false);
        return false;
      }

      params.set_params(stmt);

      result.clear();

      while(SQLITE_ROW == sqlite3_step(stmt)) //Iterate all objects
      {
          result.push_back(typename Container::value_type());
          binder.get(stmt,result.back());
      }

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
    void set_params(row) {}
  };

  template<class Params>
  static int do_execute(connection db, const char* sql, Params params)
  {
      sqlite3_stmt *stmt;
      int nResult;
      const char *pszTail;
  #if defined(_UNICODE) || defined(UNICODE)
      nResult = sqlite3_prepare16(db, sql, -1, &stmt, (void const**)&pszTail);
  #else
      nResult = sqlite3_prepare(db, sql, -1, &stmt, &pszTail);
  #endif
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
