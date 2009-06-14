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

      Console.WriteLine("Total transfers:");
      sqlgg.calc_total calc = new sqlgg.calc_total(conn);
      calc.execute(delegate (String name, decimal total) { Console.WriteLine(name + ": " + total); });

      Console.WriteLine("Donors:");
      sqlgg.list_donors donors = new sqlgg.list_donors(conn);
      donors.execute("petrov",100,delegate(string surname) { Console.WriteLine(surname); });

      conn.Close();
      conn = null;
   }
}

