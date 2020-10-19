public abstract class DataBase {
  public void createExecuteStatements() {
    System.out.println("Create Execute Statements!");
  }

  public void executeStatement() {
    System.out.println("Execute Statement!");
  }

  public void processResults() {
    System.out.println("Process Results!");
  }

  public void freeResource() {
    System.out.println("Free Resource!");
  }

  public abstract void registerDriver();

  public abstract void establishConnection();

  public void connect() {
    registerDriver();
    establishConnection();
    createExecuteStatements();
    executeStatement();
    processResults();
    freeResource();
  }
}
