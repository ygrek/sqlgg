using System.Data;
using MySql.Data.MySqlClient;

namespace sqlgg
{
  public class select_all
  {
    IDbCommand cmd;
    string sql = "SELECT name, surname FROM person LIMIT @lim";

    public select_all(IDbConnection aConn)
    {
      cmd = aConn.CreateCommand();
      cmd.CommandText = sql;
      IDbDataParameter p = cmd.CreateParameter();
      p.ParameterName = "@lim";
      p.DbType = DbType.Int32;
      cmd.Parameters.Add(p);
      cmd.Prepare();
    }

    public delegate void func(string name, string surname);

    public void execute(int limit, func f)
    {
       ((IDbDataParameter)cmd.Parameters[0]).Value = limit;
       IDataReader reader = cmd.ExecuteReader();
       while (reader.Read())
       {
            string name = (string)reader["name"];
            string surname = (string)reader["surname"];
            f(name,surname);
       }
       reader.Close();
    }
  }
}

 public class Test
 {
    public static void Main(string[] args)
    {
       string connectionString =
          "Server=localhost;" +
          "Database=test;" +
          "User ID=root;" +
          "Password=;" +
          "Pooling=false";
       IDbConnection conn = new MySqlConnection(connectionString);
       conn.Open();

       sqlgg.select_all sel = new sqlgg.select_all(conn);
       sel.execute(10, delegate (string name, string surname) {
         Console.WriteLine("Name: " + name + " " + surname); });

       // clean up
       conn.Close();
       conn = null;
    }
 }
