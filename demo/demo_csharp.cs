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

      gen.create_person();
      gen.create_money();

      gen.add_person("John","Black");
      ResultSet rs = st.executeQuery("SELECT LAST_INSERT_ID()"); rs.next();
      int john = rs.getInt(1);

      gen.add_person("Ivan","Petrov");
      rs = st.executeQuery("SELECT LAST_INSERT_ID()"); rs.next();
      int ivan = rs.getInt(1);

      gen.add_person("Sancho","Alvares");
      rs = st.executeQuery("SELECT LAST_INSERT_ID()"); rs.next();
      int sancho = rs.getInt(1);

      // add money relations
      gen.add_money(john,ivan,200);
      gen.add_money(john,sancho,100);
      gen.add_money(john,sancho,250);
      gen.add_money(sancho,ivan,300);

      Console.WriteLine("Total transfers:");
      sqlgg.calc_total calc = new sqlgg.calc_total(conn);
      calc.execute(delegate (String name, long total) { Console.WriteLine(name + ": " + total); });

      Console.WriteLine("Donors:");
      sqlgg.list_donors donors = new sqlgg.list_donors(conn);
      donors.execute("petrov",100,delegate(string surname) { Console.WriteLine(surname); });

      conn.Close();
      conn = null;
   }
}

