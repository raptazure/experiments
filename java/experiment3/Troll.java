public class Troll extends Character {
  public Troll() {
    weapon = new KnifeBehavior();
  }

  public void setWeapon(WeaponBehavior w) {
    this.weapon = w;
  }

  public void fight() {
    System.out.println("Troll fighting");
  }
}
