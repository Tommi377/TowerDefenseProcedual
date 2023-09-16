package tower

/**	Creates a player instance
  *
  * @param game					reference the the game instance this belongs to
  * @param initMoney		initial money
  * @param initLiver		initial lives
  */
class Player(val game: Game, initMoney: Int, initLives: Int) {
  
  // Player variables
  var money: Int = initMoney
  var lives: Int = initLives
  
  def damage(dmg: Int) = {
    lives -= dmg
  }
  
  def spendMoney(amount: Int) = {
    money -= amount
  }
  
  def getCredit(amount: Int) = {
    money += amount
  }
}