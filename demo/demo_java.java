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
//      System.out.println("Connected to the database");

      Statement st = db.createStatement();
      st.executeUpdate("DROP TABLE IF EXISTS person");
      st.executeUpdate("DROP TABLE IF EXISTS money");
      demo_java_gen gen = new demo_java_gen();
      gen.create_person(db);
      gen.create_money(db);

      gen.add_person(db,"John","Black");
      ResultSet rs = st.executeQuery("SELECT LAST_INSERT_ID()"); rs.next();
      int john = rs.getInt(1);

      gen.add_person(db,"Ivan","Petrov");
      rs = st.executeQuery("SELECT LAST_INSERT_ID()"); rs.next();
      int ivan = rs.getInt(1);

      gen.add_person(db,"Sancho","Alvares");
      rs = st.executeQuery("SELECT LAST_INSERT_ID()"); rs.next();
      int sancho = rs.getInt(1);

      // add money relations
      gen.add_money(db,john,ivan,200);
      gen.add_money(db,john,sancho,100);
      gen.add_money(db,john,sancho,250);
      gen.add_money(db,sancho,ivan,300);

      class a1 implements demo_java_gen.output_4
      {
        public void callback(String fullname, int total)
        {
          System.out.println(fullname + " = " + total);
        }
      }

      // summarize by person
      System.out.println("Total transfers:");
      gen.calc_total(db,new a1());

      class a2 implements demo_java_gen.output_5
      {
        public void callback(String surname)
        {
          System.out.println(surname);
        }
      }

      // list donors
      System.out.println("Donors:");
      gen.list_donors(db,new a2(),"petrov",100);

      db.close();
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }
  }

}
