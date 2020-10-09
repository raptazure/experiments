public class Oracle extends Base {
  public void register() {
    System.out.println("Registration Oracle Driver");
  }

  public void establish() {
    System.out.println("Establish Oracle Connection");
  }

  public void connect() {
    register();
    establish();
    create();
    execute();
    process();
    free();
  }
}
