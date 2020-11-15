public class Queen extends Character {
  public Queen() {
    weapon = new BowAndArrowBehavior();
  }

  public void setWeapon(WeaponBehavior w) {
    this.weapon = w;
  }

  public void fight() {
    System.out.println("Queen fighting");
  }
}
