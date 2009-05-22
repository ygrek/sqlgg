#include <mysql/mysql.h>
//#if defined(_UNICODE)
//#define TCHAR wchar_t
//#define _T(x) L##x
//#else
#define TCHAR char
#define _T(x) x
//#endif
#include <assert.h>
#include <string>
#include <string.h>

struct mysql_traits
{
  typedef int Int;
  typedef std::basic_string<TCHAR> Text;
  typedef Text Any;

  typedef MYSQL_STMT* statement;
  typedef MYSQL* connection;

  /*
     template<class T>
     static void get_column_Int(statement stmt, int index, T& val)
     {
     val = sqlite3_column_int(stmt, index);
     }

     template<class T>
     static void get_column_Text(statement stmt, int index, T& val)
     {
#if defined(_UNICODE) || defined(UNICODE)
val = (const TCHAR*)sqlite3_column_text16(stmt, index);
#else   
val = (const TCHAR*)sqlite3_column_text(stmt, index);
#endif   
}
*/

  /*
     static void set_param_null(statement stmt, int index)
     {
     int nResult = sqlite3_bind_null(stmt, index + 1);
     ATLASSERT(SQLITE_OK == nResult);
     }

     static void set_param_Text(statement stmt, const Text& val, int index)
     {
#if defined(_UNICODE) || defined(UNICODE)
int nResult = sqlite3_bind_text16(stmt, index + 1, val.c_str(), -1, SQLITE_TRANSIENT);
#else
int nResult = sqlite3_bind_text(stmt, index + 1, val.c_str(), -1, SQLITE_TRANSIENT);
#endif     
ATLASSERT(SQLITE_OK == nResult);
}

static void set_param_Any(statement stmt, const Any& val, int index)
{
set_param_Text(stmt,val,index);
}

static void set_param_Int(statement stmt, const Int& val, int index)
{
int nResult = sqlite3_bind_int(stmt, index + 1, val);
ATLASSERT(SQLITE_OK == nResult);
}
*/

// FIXME destroy stmt on error
template<class Container, class Binder, class Params>
static bool do_select(connection db, Container& result, const TCHAR* sql, Binder binder, Params params)
{
  MYSQL_STMT* stmt = mysql_stmt_init(db);
  if (!stmt)
  {
    fprintf(stderr, " mysql_stmt_init(), out of memory\n");
    return false;
  }
  if (mysql_stmt_prepare(stmt, sql, strlen(sql)))
  {
    fprintf(stderr, " mysql_stmt_prepare(), SELECT failed\n");
    fprintf(stderr, " %s\n", mysql_stmt_error(stmt));
    return false;
  }
  if (Params::count != mysql_stmt_param_count(stmt))
  {
    fprintf(stderr, " wrong params count\n");
    return false;
  }
  if (Binder::count != mysql_stmt_field_count(stmt))
  {
    fprintf(stderr, " wrong number of columns\n");
    return false;
  }

  //params.set_params(stmt);
  if (mysql_stmt_execute(stmt))
  {
    fprintf(stderr, " mysql_stmt_execute(), failed\n");
    fprintf(stderr, " %s\n", mysql_stmt_error(stmt));
    return false;
  }

//  result.clear();

//  while(SQLITE_ROW == sqlite3_step(stmt)) //Iterate all objects
//  {
//    result.push_back(typename Container::value_type());
//    binder.of_stmt(stmt,result.back());
//  }

  //Destroy the command
  if (mysql_stmt_close(stmt))
  {
    fprintf(stderr, " failed while closing the statement\n");
    fprintf(stderr, " %s\n", mysql_stmt_error(stmt));
    return false;
  }

  return true;
}

struct no_params
{
  void set_params(statement) {}
  enum { count = 0 };
};

template<class Params>
static bool do_execute(connection db, const char* sql, Params params)
{
  no_params z;
  return do_select(db,z,sql,z,params);
}

}; // sqlite3_traits
