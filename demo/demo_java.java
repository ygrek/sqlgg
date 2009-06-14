import java.sql.*;

public class demo_java
{
  public static void main(String[] args)
  {
    String url = "jdbc:mysql://localhost:3306/";
    String dbName = "test";
    String driver = "com.mysql.jdbc.Driver";
    String userName = "root";
    String password = "";
    try
    {
      Class.forName(driver).newInstance();
      Connection db = DriverManager.getConnection(url+dbName,userName,password);

      Statement st = db.createStatement();
      st.executeUpdate("DROP TABLE IF EXISTS person");
      st.executeUpdate("DROP TABLE IF EXISTS money");
      demo_java_gen gen = new demo_java_gen(db);
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

      class a1 implements demo_java_gen.calc_total_callback
      {
        public void callback(String fullname, int total)
        {
          System.out.println(fullname + " = " + total);
        }
      }

      // summarize by person
      System.out.println("Total transfers:");
      gen.calc_total(new a1());

      class a2 implements demo_java_gen.list_donors_callback
      {
        public void callback(String surname)
        {
          System.out.println(surname);
        }
      }

      // list donors
      System.out.println("Donors:");
      gen.list_donors("petrov",100,new a2());

      db.close();
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }
  }

}
