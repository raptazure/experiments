public class Client {
  public static void main(String[] args) {
    MySql db1 = new MySql();
    Oracle db2 = new Oracle();
    System.out.println(">>= Testing MySQL");
    db1.connect();
    System.out.println(">>= Testing Oracle");
    db2.connect();
  }
}
