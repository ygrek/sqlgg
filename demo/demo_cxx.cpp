#include "../impl/sqlite3_traits.hpp" // sqlite3 traits
#include "demo_cxx_gen.hpp" // generated
#include <iostream>
#include <vector>

using namespace std;

typedef sqlgg<sqlite3_traits> gen_t;
typedef long long int64;

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
  sqlite3* db = NULL;
  sqlite3_open(":memory:", &db);

  gen_t gen(db);

  // create tables
  gen.create_person();
  gen.create_money();

  // add all person records
  gen.add_person("John","Black");
  int64 john = sqlite3_last_insert_rowid(db);
  gen.add_person("Ivan","Petrov");
  int64 ivan = sqlite3_last_insert_rowid(db);
  gen.add_person("Sancho","Alvares");
  int64 sancho = sqlite3_last_insert_rowid(db);

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
  sqlite3_close(db);

  return 0;
}
