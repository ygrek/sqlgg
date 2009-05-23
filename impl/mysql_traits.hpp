#include <mysql/mysql.h>
//#if defined(_UNICODE)
//#define TCHAR wchar_t
//#define _T(x) L##x
//#else
#define TCHAR char
#define _T(x) x
//#endif
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include <string>
#include <vector>

struct mysql_traits
{
  typedef int Int;
  typedef std::basic_string<TCHAR> Text;
  typedef Text Any;

  struct row_t
  {
    template<class T> T* alloc(size_t const count) { return (T*)calloc(count,sizeof(T)); }

    row_t(MYSQL_STMT* stmt, size_t count) : count(count), stmt(stmt)
    {
      bind = alloc<MYSQL_BIND>(count);
      error = alloc<my_bool>(count);
      length = alloc<unsigned long>(count);
      is_null = alloc<my_bool>(count);
    }

    ~row_t()
    {
      free(bind);
      free(error);
      free(length);
      free(is_null);
    }

    const size_t count;
    MYSQL_STMT* stmt;

    MYSQL_BIND* bind;
    unsigned long* length;
    my_bool* error;
    my_bool* is_null;
  };

  typedef row_t& row;
  typedef MYSQL* connection;

  static void get_column(row r, int index, Int& data)
  {
    // nothing
  }

  static void get_column(row r, int index, Text& data)
  {
    unsigned long const& length = r.length[index];
    MYSQL_BIND& bind = r.bind[index];

    if (length > 0)
    {
      data.resize(length);
      bind.buffer = (void*)(data.c_str());
      bind.buffer_length = length;
      mysql_stmt_fetch_column(r.stmt, r.bind, index, 0);
    }

    //data.resize(length);
  }

  static void bind_column(row r, int index, Int& data)
  {
    MYSQL_BIND& bind = r.bind[index];

    bind.buffer_type = MYSQL_TYPE_LONG;
    bind.buffer = (void*)&data;
    bind.is_null = &r.is_null[index];
    bind.length = &r.length[index];
    bind.error = &r.error[index];
  }

  static void bind_column(row r, int index, Text& data)
  {
    MYSQL_BIND& bind = r.bind[index];

    bind.buffer_type = MYSQL_TYPE_STRING;
    bind.buffer = 0;
    bind.buffer_length = 0;
    bind.is_null = &r.is_null[index];
    bind.length = &r.length[index];
    bind.error = &r.error[index];
  }

  static void set_param_Text(row r, const Text& val, int index)
  {
    MYSQL_BIND& bind = r.bind[index];

    r.length[index] = val.size();
    bind.length = &r.length[index];
    bind.buffer_length = val.size();
    bind.buffer_type = MYSQL_TYPE_STRING;
    bind.buffer = (void*)val.c_str();
  }

  static void set_param_Any(row r, const Any& val, int index)
  {
    set_param_Text(r,val,index);
  }

  static void set_param_Int(row r, const Int& val, int index)
  {
    MYSQL_BIND& bind = r.bind[index];

    bind.buffer_type = MYSQL_TYPE_LONG;
    bind.buffer = (void*)&val;
  }

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

  row_t r_params(stmt,Params::count);
  params.set_params(r_params);

  if (mysql_stmt_bind_param(stmt, r_params.bind))
  {
    fprintf(stderr, " mysql_stmt_bind_param() failed\n");
    fprintf(stderr, " %s\n", mysql_stmt_error(stmt));
    return false;
  }

  if (mysql_stmt_execute(stmt))
  {
    fprintf(stderr, " mysql_stmt_execute(), failed\n");
    fprintf(stderr, " %s\n", mysql_stmt_error(stmt));
    return false;
  }

  row_t r(stmt,Binder::count);
  typename Container::value_type val;
  binder.bind(r,val);

  result.clear();
  while (0 == mysql_stmt_fetch(stmt))
  {
    binder.get(r,val);
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
  void set_params(row) {}
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
  std::vector<int> z;
  return do_select(db,z,sql,no_binder<int>(),params);
}

}; // mysql_traits

