#include <iostream>
#include <pqxx/pqxx>
#include "pqxx_traits.hpp"
#include "test_gen.hpp"

using namespace std;
using namespace pqxx;

typedef sqlgg<pqxx_traits> gen;

int main()
{
	try
	{
		connection C("dbname=test");
		cout << "Connected to " << C.dbname() << endl;
		work W(C);

    result R;
    R = W.exec("DROP TABLE employee");
    gen::create_employee(W);

    gen::insert_employee(W,"john",2);
    gen::insert_employee(W,"jack",3);
    gen::insert_employee(W,"bob",4);
		R = W.exec("SELECT name FROM employee");

		cout << "Found " << R.size() << " employees:" << endl;
		for (result::const_iterator r = R.begin();
		     r != R.end();
		     ++r)
		{
			cout << r[0].c_str() << endl;
		}

		cout << "Doubling all employees' salaries..." << endl;
		W.exec("UPDATE employee SET salary=salary*2");

		cout << "Making changes definite: ";
		W.commit();
		cout << "ok." << endl;
	}
	catch (const exception &e)
	{
		cerr << e.what() << endl;
		return 1;
	}
	return 0;
}
