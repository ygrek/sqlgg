#include "../impl/mysql_traits.hpp" // mysql traits
#include "demo_cxx_gen.hpp" // generated
#include <iostream>
#include <vector>

using namespace std;

typedef sqlgg<mysql_traits> gen;
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

int main()
{
  MYSQL* db = connect();
  if (!db) return 1;

  mysql_query(db,"DROP TABLE person");
  mysql_query(db,"DROP TABLE money");

  // create tables
  gen::create_person(db);
  gen::create_money(db);

  // add all person records
  gen::add_person(db,"John","Black");
  int64 john = mysql_insert_id(db);
  gen::add_person(db,"Ivan","Petrov");
  int64 ivan = mysql_insert_id(db);
  gen::add_person(db,"Sancho","Alvares");
  int64 sancho = mysql_insert_id(db);

  // add money relations
  gen::add_money(db,john,ivan,200);
  gen::add_money(db,john,sancho,100);
  gen::add_money(db,john,sancho,250);
  gen::add_money(db,sancho,ivan,300);

  // summarize by person
  typedef vector<gen::data_4> collection;
  collection all;
  gen::calc_total(db,all);

  // output
  cout << "Total transfers:" << endl;
  for (collection::const_iterator i = all.begin(), end = all.end(); i != end; ++i)
  {
     cout << i->fullname << " = " << i->total << endl;
  }

  // list donors
  typedef vector<gen::data_5> people;
  people p;
  gen::list_donors(db,p,"petrov",100);

  cout << "Donors:" << endl;
  for (people::const_iterator i = p.begin(), end = p.end(); i != end; ++i)
  {
    cout << i->surname << endl;
  }

  // properly close database
  mysql_close(db);
  db = NULL;

  return 0;
}
