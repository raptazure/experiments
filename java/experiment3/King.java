public class King extends Character {
  public King() {
    weapon = new SwordBehavior();
  }

  public void setWeapon(WeaponBehavior w) {
    this.weapon = w;
  }

  public void fight() {
    System.out.println("King fighting");
  }
}
