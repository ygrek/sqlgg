using System.Data;
using MySql.Data.MySqlClient;

public class Test
{
   public static void Main(string[] args)
   {
      string connectionString = "Server=localhost;Database=test;User ID=root;Password=;";
      IDbConnection conn = new MySqlConnection(connectionString);
      conn.Open();

      sqlgg.calc_total calc = new sqlgg.calc_total(conn);
      calc.execute(delegate (string name, int total) { System.Console.WriteLine(name + ": " + total); });

      conn.Close();
      conn = null;
   }
}

