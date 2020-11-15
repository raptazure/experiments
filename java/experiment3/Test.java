public class Test {
  public static void main(String args[]) {
    Character king = new King();
    Character queen = new Queen();
    Character knight = new Knight();
    Character troll = new Troll();
    WeaponBehavior axe = new AxeBehavior();
    WeaponBehavior sword = new SwordBehavior();
    WeaponBehavior knife = new KnifeBehavior();
    WeaponBehavior bowAndArrow = new BowAndArrowBehavior();
    king.weapon.useWeapon();
    king.setWeapon(axe);
    king.weapon.useWeapon();
    king.fight();

    queen.weapon.useWeapon();
    queen.setWeapon(sword);
    queen.weapon.useWeapon();
    queen.fight();

    knight.weapon.useWeapon();
    knight.setWeapon(knife);
    knight.weapon.useWeapon();
    knight.fight();

    troll.weapon.useWeapon();
    troll.setWeapon(bowAndArrow);
    troll.weapon.useWeapon();
    troll.fight();
  }
}
