/*
  Mysql C++ traits for sqlgg
  by ygrek
  2014-06-08

  This is free and unencumbered software released into the public domain.

  Anyone is free to copy, modify, publish, use, compile, sell, or
  distribute this software, either in source code form or as a compiled
  binary, for any purpose, commercial or non-commercial, and by any
  means.

  For more information, please refer to <http://unlicense.org/>
*/

#include <mysql/mysql.h>
#define SQLGG_STR(x) x

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include <string>
#include <vector>

#if defined(SQLGG_DEBUG)
#include <stdio.h>
#endif

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
    void get(row,T) {}
    void bind(row) {}
    enum { count = 0 };
  };


  class statement
  {
  private:
    connection db;
    char const* sql;
    mysql_stmt stmt;
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

    template<class T, class Binder, class Params>
    bool select(T result, Binder binder, Params params)
    {
      prepare();

      if (Params::count != mysql_stmt_param_count(stmt))
      {
#if defined(SQLGG_DEBUG)
        fprintf(stderr, " wrong params count %u != %lu\n",Params::count,mysql_stmt_param_count(stmt));
#endif
        return false;
      }
      if (Binder::count != mysql_stmt_field_count(stmt))
      {
#if defined(SQLGG_DEBUG)
        fprintf(stderr, " wrong number of columns %u != %u\n",Binder::count,mysql_stmt_field_count(stmt));
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

      if (0 != Binder::count)
      {
        binder.bind(r);

        if (mysql_stmt_bind_result(stmt, r.bind))
        {
#if defined(SQLGG_DEBUG)
          fprintf(stderr, " mysql_stmt_bind_result() failed\n");
          fprintf(stderr, " %s\n", mysql_stmt_error(stmt));
#endif
          return false;
        }
      }

      while (true)
      {
        int res = mysql_stmt_fetch(stmt);
        if (0 != res && MYSQL_DATA_TRUNCATED != res) break;
        binder.get(r,result);
      }

      return true;
    }

    template<class Params>
    bool execute(Params params)
    {
      int dummy = 0;
      return select(dummy,no_binder<int>(),params);
    }

  }; // statement

}; // mysql_traits
