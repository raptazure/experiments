public class Oracle {
  public void register() {
    System.out.println("Registration Oracle Driver");
  }

  public void establish() {
    System.out.println("Establish Oracle Connection");
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
