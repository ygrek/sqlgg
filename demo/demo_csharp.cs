using System;
using System.Data;
using MySql.Data.MySqlClient;

public class Test
{
   public static void Main(string[] args)
   {
      string connectionString = "Server=localhost;Database=test;User ID=root;Password=;";
      IDbConnection conn = new MySqlConnection(connectionString);
      conn.Open();

      sqlgg.all gen = new sqlgg.all(conn);

      gen.drop_person.execute();
      gen.drop_money.execute();

      gen.create_person.execute();
      gen.create_money.execute();

      IDbCommand last_id = conn.CreateCommand();
      last_id.CommandText = "SELECT LAST_INSERT_ID()";
      last_id.Prepare();

      gen.add_person.execute("John","Black");
      long john = (long)last_id.ExecuteScalar();

      gen.add_person.execute("Ivan","Petrov");
      long ivan = (long)last_id.ExecuteScalar();

      gen.add_person.execute("Sancho","Alvares");
      long sancho = (long)last_id.ExecuteScalar();

      // add money relations
      gen.add_money.execute(john,ivan,200);
      gen.add_money.execute(john,sancho,100);
      gen.add_money.execute(john,sancho,250);
      gen.add_money.execute(sancho,ivan,300);

      Console.WriteLine("Total transfers:");
      foreach (var row in gen.calc_total.rows())
      {
        Console.WriteLine(row.fullname + ": " + row.total);
      }

      Console.WriteLine("Donors:");
      gen.list_donors.execute("petrov",100,delegate(string surname) { Console.WriteLine(surname); });

      conn.Close();
      conn = null;
   }
}

