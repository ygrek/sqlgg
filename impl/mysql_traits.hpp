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

  template<size_t count>
  struct rowT
  {
    rowT(MYSQL_STMT* stmt) : stmt(stmt)
    {
      memset(bind,0,sizeof(bind));
      memset(length,0,sizeof(length));
      memset(error,0,sizeof(error));
      memset(is_null,0,sizeof(is_null));
    }

    MYSQL_STMT* stmt;
    MYSQL_BIND bind[count];
    unsigned long length[count];
    my_bool error[count];
    my_bool is_null[count];
  }

  typedef row& row;
  typedef MYSQL* connection;

  template<class T>
  static void get_column(statement stmt, int index, T& data)
  {
    // nothing
  }

  template<>
  static void get_column(statement stmt, int index, Text& data)
  {
    unsigned long const& length = stmt.length[index];
    MYSQL_BIND& bind = stmt.bind[index];

    if (length > 0)
    {
      data.resize(length);
      bind.buffer = data.c_str();
  bind.buffer_length = length;
  mysql_stmt_fetch_column(stmt, stmt.bind, index, 0);
}

    data.resize(stmt.length[index])
  }

  static void bind_column(statement stmt, int index, Int& data)
  {
    MYSQL_BIND& bind = stmt.bind[index];

    bind.buffer_type = MYSQL_TYPE_LONG;
    bind.buffer = (void*)&data;
    bind.is_null = &stmt.is_null[index];
    bind.length = &stmt.length[index];
    bind.error = &stmt.error[index];
  }

  static void bind_column(statement stmt, int index, Text& data)
  {
    // FIXME
    data.resize(1024);

    MYSQL_BIND& bind = stmt[index];

    bind.buffer_type = MYSQL_TYPE_STRING;
    bind.buffer = data.c_str();
    bind.buffer_length = 1024;
    bind.is_null = &stmt.is_null[index];
    bind.length = &stmt.length[index];
    bind.error = &stmt.error[index];
  }

  /*
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

  row<Binder::count> data;
  Container::value_type val;
  binder.bind(data,val);

  result.clear();
  while (0 == mysql_stmt_fetch(stmt))
  {
    binder.get(data,val);
    result.push_back(val);
  }

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
