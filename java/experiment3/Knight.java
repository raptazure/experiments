public class Knight extends Character {
  public Knight() {
    weapon = new AxeBehavior();
  }

  public void setWeapon(WeaponBehavior w) {
    this.weapon = w;
  }

  public void fight() {
    System.out.println("Knight fighting");
  }
}
