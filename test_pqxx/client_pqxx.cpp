#include <iostream>
#include <pqxx/pqxx>

using namespace std;
using namespace pqxx;

int main()
{
	try
	{
		connection C("dbname=test");
		cout << "Connected to " << C.dbname() << endl;
		work W(C);

    result R;
    R = W.exec("DROP TABLE employee");
    R = W.exec("CREATE TABLE employee (id SERIAL PRIMARY KEY, name TEXT, salary INT)");

    C.prepare("ins","INSERT INTO employee (name,salary) VALUES($1,$2)")
        ("varchar",prepare::treat_string)
        ("varchar",prepare::treat_direct);
    prepare::invocation* call = &W.prepared("ins");
    call->operator()("john")(2).exec();
    /*
    R = W.prepared("ins")("jack")(3).exec();
    R = W.prepared("ins")("bob")(4).exec();
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
    */
		cout << "ok." << endl;
	}
	catch (const exception &e)
	{
		cerr << e.what() << endl;
		return 1;
	}
	return 0;
}
