#include "../impl/sqlite3_traits.hpp" // sqlite3 traits
#include "demo_cxx_gen.hpp" // generated
#include <iostream>
#include <vector>

using namespace std;

typedef sqlgg<sqlite3_traits> gen;
typedef long long int64;

int main()
{
  sqlite3* db = NULL;
  sqlite3_open(":memory:", &db);

  // create tables
  gen::create_person(db);
  gen::create_money(db);

  // add all person records
  gen::add_person(db,"John","Black");
  int64 john = sqlite3_last_insert_rowid(db);
  gen::add_person(db,"Ivan","Petrov");
  int64 ivan = sqlite3_last_insert_rowid(db);
  gen::add_person(db,"Sancho","Alvares");
  int64 sancho = sqlite3_last_insert_rowid(db);

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
  sqlite3_close(db);

  return 0;
}
