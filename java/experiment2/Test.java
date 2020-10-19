public class Test {
  public static void main(String args[]) {
    System.out.println(">>= Testing MySQL");
    MySQL db1 = new MySQL();
    db1.connect();
    System.out.println(">>= Testing Oracle");
    Oracle db2 = new Oracle();
    db2.connect();
    System.out.println(">>= Testing Access");
    Access db3 = new Access();
    db3.connect();
  }
}
