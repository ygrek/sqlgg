import java.sql.*;

public class test
{
  public static void main(String[] args)
  {
    System.out.println("MySQL Connect Example.");
    Connection conn = null;
    String url = "jdbc:mysql://localhost:3306/";
    String dbName = "test";
    String driver = "com.mysql.jdbc.Driver";
    String userName = "root";
    String password = "";
    try
    {
      Class.forName(driver).newInstance();
      conn = DriverManager.getConnection(url+dbName,userName,password);
      System.out.println("Connected to the database");

      Statement st = conn.createStatement();
      st.executeUpdate("DROP TABLE IF EXISTS person");
      st.executeUpdate("DROP TABLE IF EXISTS money");
      sqlgg.create_person(conn);
      sqlgg.create_money(conn);
      conn.close();
      System.out.println("Disconnected from database");
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

}
