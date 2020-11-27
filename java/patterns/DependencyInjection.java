public class DependencyInjection {
  // based on constructor
  public static void main(String[] args) {
    AnotherRandomPerson rp = new AnotherRandomPerson(new Three());
    rp.command();
  }
}

public interface Cleaner {
  void sweep();
}

public class Two implements Cleaner {
  @Override
  public void sweep() {
    System.out.println("two is sweeping!");
  }

  public boolean isTraining() {
    return false;
  }
}

public class Three implements Cleaner {
  @Override
  public void sweep() {
    System.out.printf("three is sweeping!");
  }
}

public class Watcher {
  public static Cleaner getCleaner() {
    Two two = new Two();
    if (two.isTraining()) {
      return new Three();
    }

    return two;
  }
}

public class RandomPerson {
  public void command() {
    Watcher.getCleaner().sweep();
  }
}

public class Ioc {
  public void main(String[] args) {
    RandomPerson rp = new RandomPerson();
    rp.command();
  }
}

public class AnotherRandomPerson {
  private Cleaner sweeper;

  public AnotherRandomPerson(Cleaner sweeper) {
    this.sweeper = sweeper;
  }

  public void command() {
    this.sweeper.sweep();
  }
}