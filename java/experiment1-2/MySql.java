public class MySql extends Base {
  public void register() {
    System.out.println("Registration MySQL Driver");
  }

  public void establish() {
    System.out.println("Establish MySQL Connection");
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