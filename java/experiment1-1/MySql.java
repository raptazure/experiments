public class MySql {
  public void register() {
    System.out.println("Registration MySQL Driver");
  }

  public void establish() {
    System.out.println("Establish MySQL Connection");
  }

  public void create() {
    System.out.println("Create Execute Statements");
  }

  public void execute() {
    System.out.println("Execute Statement");
  }

  public void process() {
    System.out.println("Processing Results");
  }

  public void free() {
    System.out.println("Free Resource");
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