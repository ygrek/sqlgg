SQL to C++ code generator
=========================

Problem
-------

Writing database layer code is usually tedious and error-prone, due to the mix of different
languages. SQL queries constructed dynamically need to bind external data (from application), and
the resulting rowset must be decomposed into application native data.  Data crossing these
database-application boundaries is what causes troubles. One can factor out all common database
communication code, hide the database under some application-specific abstraction, but one always
needs to manully specify correspondence between SQL query binding slots (or resulting rowset
columns) and code variables. This mapping should be updated manually every time SQL query is
modified. 

Solution
--------

SQL parser and code generator which ensures that application code and database queries are in sync.
It analyzes SQL query and determines from it the set of input parameters (values for INSERT,
run-time substitution parameters) and the set of resulting columns (from SELECT). Then it generates
the C++ code which structures input and output values together and assignes corresponding native data
types. So basically you provide an SQL query and get a C++ function which takes the set of
parameters as required to fill slots in a query, combines them with your query and executes it.
SELECT statements additionally return the collection of structures with fields representing columns
of resulting rowset. So if you modify the query and forget to update the code -- the compiler will point 
on errorneous parts.

Example
-------

Queries:

    CREATE TABLE test (id INTEGER PRIMARY KEY AUTOINCREMENT,name TEXT,descr TEXT);
    INSERT INTO test(name,descr) VALUES (?,?);
    SELECT name,descr FROM test WHERE name = @name LIMIT @limit;
    SELECT name,z FROM 
      (SELECT name,
        city || ' ' || descr as y,
        max(length(city),random(*)) as z 
        FROM test LEFT JOIN 
          (SELECT name AS city FROM test WHERE id=@id))
    WHERE x < @level;

Generated code (only prototypes):

    template <class Traits>
    struct sql2cpp
    {
      static bool create_test(typename Traits::connection db);
      static bool insert_2(typename Traits::connection db, typename Traits::Text const& name, typename Traits::Text const& descr);

      struct data_3
      {
        typename Traits::Int id;
        typename Traits::Text name;
        typename Traits::Text descr;
      }; // struct data_3

      template<class T>
      static bool select_3(typename Traits::connection db, T& result, typename Traits::Any const&	name, typename Traits::Int const& limit);

      struct data_4
      {
        typename Traits::Text name;
        typename Traits::Text z;
      }; // struct data_4

      template<class T>
      static bool select_4(typename Traits::connection db, T& result, typename Traits::Any const& id, typename Traits::Any const& level);
    }

Things to note above:

1. The generated code is parametrized by database-specific class `Traits`. It specifies the
		corresponding between SQL and native types, provides types for database connection and other
		details. `Traits` also implements actual code to execute statements. It should be implemented
		once for every specific database API.
2. `insert_2()` requires two data parameters, the values to INSERT into table, the binding into
		statement is done behind the scenes.
3. `select_3()` returns data via `result` parameter. It is not visible above, but `T` should be a
		container with `T::value_type` having at least the fields of `data_3` (otherwise it will fail to
		compile), e.g. `std::vector<data_3>` is fine.
4. The type of limit is `Traits::Int` as expected, though the type of `name` is `Any` which is
		unfortunate. In the future it will be possible to infer a specific type.
5. Statements can be of arbitrary depth across many tables.
6. Statements are checked for correctness as far as generator is concerned, so it will detect
		syntax errors, non-existant columns in expressions, mismatched rowsets in compound statements,
		ambigous column names etc.

Details
-------

This is work in progress and there is plenty of room for improvement. 
The generator is already used for some simple database-access code. It uses
[sqlite3](http://sqlite.org) as a database and implements suitable `sqlite3_traits`
helper. 

Online version will be made available soon.

TODO
----

* type check expressions, infer type for parameters
* validate expressions with regard to scheme in their scope
* detect statements on single tables and group the corresponding generated code in one class
* check names (functions and bindings) for uniqueness
* support/test other SQL engines
* generate code for more languages
* read SQL spec

----
2009-05-03

<style>
code { font-family: monospace; font-style: italic; }
pre { background-color: #eee; color: black; border: 1px dashed #0c5; }
/*body { padding-bottom: 200px; }*/
</style>
