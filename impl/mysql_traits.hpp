#include <mysql/mysql.h>
#define SQLGG_STR(x) x

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include <string>
#include <vector>

struct mysql_traits
{
  typedef int Int;
  typedef std::string Text;
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

  static void get_column_Int(row r, int index, Int& data)
  {
    // nothing
  }

  static void get_column_Text(row r, int index, Text& data)
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

  static void bind_column_Int(row r, int index, Int& data)
  {
    MYSQL_BIND& bind = r.bind[index];

    data = 0;
    bind.buffer_type = MYSQL_TYPE_LONG;
    bind.buffer = (void*)&data;
    bind.is_null = &r.is_null[index];
    bind.length = &r.length[index];
    bind.error = &r.error[index];
  }

  static void bind_column_Text(row r, int index, Text& data)
  {
    MYSQL_BIND& bind = r.bind[index];

    bind.buffer_type = MYSQL_TYPE_STRING;
    bind.buffer = 0;
    bind.buffer_length = 0;
    bind.is_null = &r.is_null[index];
    bind.length = &r.length[index];
    bind.error = &r.error[index];
  }

  static void set_param(row r, const Text& val, int index)
  {
    MYSQL_BIND& bind = r.bind[index];

    r.length[index] = val.size();
    bind.length = &r.length[index];
    bind.buffer_length = val.size();
    bind.buffer_type = MYSQL_TYPE_STRING;
    bind.buffer = (void*)val.c_str();
  }

  static void set_param(row r, const Int& val, int index)
  {
    MYSQL_BIND& bind = r.bind[index];

    bind.buffer_type = MYSQL_TYPE_LONG;
    bind.buffer = (void*)&val;
  }

  class mysql_stmt
  {
  public:
    mysql_stmt(MYSQL_STMT* stmt) : stmt(stmt)
    {
    }

    void close()
    {
      if (stmt) mysql_stmt_close(stmt);
      stmt = NULL;
    }

    virtual ~mysql_stmt()
    {
      close();
    }

/*
    mysql_stmt& operator=(MYSQL_STMT* v)
    {
      close();
      stmt = v;
    }
*/

    operator MYSQL_STMT*()
    {
      return stmt;
    }

  private:
    MYSQL_STMT* stmt;
  };

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


  class statement
  {
  private:
    mysql_stmt stmt;
    char const* sql;
    connection db;
    bool ready;

  public:
    statement(connection aDb, char const* sql) : db(aDb), sql(sql), stmt(mysql_stmt_init(aDb))
    {
      ready = false;
    }

    bool prepare()
    {
      if (ready)
      {
        return true;
      }

      if (!stmt)
      {
#if defined(SQLGG_DEBUG)
        fprintf(stderr, " mysql_stmt_init(), out of memory\n");
#endif
        return false;
      }

      if (mysql_stmt_prepare(stmt, sql, strlen(sql)))
      {
#if defined(SQLGG_DEBUG)
        fprintf(stderr, " mysql_stmt_prepare(), failed\n");
        fprintf(stderr, " %s\n", mysql_stmt_error(stmt));
#endif
        return false;
      }

      ready = true;
      return true;
    }

    template<class Container, class Binder, class Params>
    bool select(Container& result, Binder binder, Params params)
    {
      if (Params::count != mysql_stmt_param_count(stmt))
      {
#if defined(SQLGG_DEBUG)
        fprintf(stderr, " wrong params count\n");
#endif
        return false;
      }
      if (Binder::count != mysql_stmt_field_count(stmt))
      {
#if defined(SQLGG_DEBUG)
        fprintf(stderr, " wrong number of columns\n");
#endif
        return false;
      }

      row_t r_params(stmt,Params::count);
      params.set_params(r_params);

      if (mysql_stmt_bind_param(stmt, r_params.bind))
      {
#if defined(SQLGG_DEBUG)
        fprintf(stderr, " mysql_stmt_bind_param() failed\n");
        fprintf(stderr, " %s\n", mysql_stmt_error(stmt));
#endif
        return false;
      }

      if (mysql_stmt_execute(stmt))
      {
#if defined(SQLGG_DEBUG)
        fprintf(stderr, " mysql_stmt_execute(), failed\n");
        fprintf(stderr, " %s\n", mysql_stmt_error(stmt));
#endif
        return false;
      }

      row_t r(stmt,Binder::count);
      typename Container::value_type val;
      binder.bind(r,val);

      if (0 != Binder::count)
      {
        if (mysql_stmt_bind_result(stmt, r.bind))
        {
#if defined(SQLGG_DEBUG)
          fprintf(stderr, " mysql_stmt_bind_result() failed\n");
          fprintf(stderr, " %s\n", mysql_stmt_error(stmt));
#endif
          return false;
        }
      }

      result.clear();
      while (true)
      {
        int res = mysql_stmt_fetch(stmt);
        if (0 != res && MYSQL_DATA_TRUNCATED != res) break;
        binder.get(r,val);
        result.push_back(val);
      }

      return true;
    }

    template<class Params>
    bool execute(Params params)
    {
      std::vector<int> z;
      return do_select(db,z,sql,no_binder<int>(),params);
    }

  }; // statement

}; // mysql_traits

