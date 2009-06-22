#include "../impl/mysql_traits.hpp" // mysql traits
#include "demo_cxx_gen_mysql.hpp" // generated
#include <iostream>
#include <vector>

using namespace std;

typedef sqlgg<mysql_traits> gen_t;
typedef long long int64;

MYSQL* connect()
{
  MYSQL* conn = mysql_init (NULL);
  if (conn == NULL)
  {
    fprintf (stderr, "mysql_init() failed (probably out of memory)\n");
    return NULL;
  }
  if (mysql_real_connect (
        conn,          /* pointer to connection handler */
        NULL, /* host to connect to */
        "root", /* user name */
        NULL,  /* password */
        "test",   /* database to use */
        0,             /* port (use default) */
        NULL,          /* socket (use default) */
        0)             /* flags (none) */
      == NULL)
  {
    fprintf (stderr, "mysql_real_connect() failed:\nError %u (%s)\n",
              mysql_errno (conn), mysql_error (conn));
    return NULL;
  }
  return conn;
}

struct output_transfers
{
  void operator()(std::string const& fullname, int total)
  {
     cout << fullname << " = " << total << endl;
  }
};

struct output_donors
{
  void operator()(std::string const& surname)
  {
     cout << surname << endl;
  }
};

int main()
{
  MYSQL* db = connect();
  if (!db) return 1;

  gen_t gen(db);

  mysql_query(db,"DROP TABLE person");
  mysql_query(db,"DROP TABLE money");

  // create tables
  gen.create_person();
  gen.create_money();

  // add all person records
  gen.add_person("John","Black");
  int64 john = mysql_insert_id(db);
  gen.add_person("Ivan","Petrov");
  int64 ivan = mysql_insert_id(db);
  gen.add_person("Sancho","Alvares");
  int64 sancho = mysql_insert_id(db);

  // add money relations
  gen.add_money(john,ivan,200);
  gen.add_money(john,sancho,100);
  gen.add_money(john,sancho,250);
  gen.add_money(sancho,ivan,300);

  // summarize by person
  cout << "Total transfers:" << endl;
  gen.calc_total(output_transfers());

  // list donors
  cout << "Donors:" << endl;
  gen.list_donors("petrov",100,output_donors());

  // properly close database
  mysql_close(db);
  db = NULL;

  return 0;
}
