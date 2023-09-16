package tower

import java.awt.Graphics2D
import java.awt.Color
import scala.collection.mutable.Buffer

/**	Creates a enemy instance
  *
  * @param game				reference the the game instance this belongs to
  * @param speed			speed of the enemy. Lower is faster (the value is literally the idle time between moves)
  * @param maxhp			maximum hp of the enemy
  * @param damage			damage the enemy deals
  * @param color			color of the enemy
  * @param value			value of the enemy. The amount rewarded to the player if they kill this enemy
  */
class Enemy(val game: Game, val speed: Int, val maxhp: Int, val damage: Int, val color: Color, val value: Int) {
  def this(game: Game, i: EnemyInfo) = this(game, i.speed.get, i.maxhp.get, i.damage.get, i.color.get, i.value.get)
  
  var coord = Coord(0, 0)
  var path: Buffer[Coord] = Buffer()
  
  // Increments for each frame idle
  private var idleCount = 0
  
  // path holds a reference which is shared by many different instances 
  // which means that we can't mutate it so we'll use an index instead
  private var pathIndex: Int = 0
  
  var hp = maxhp
  
  
  /**	Draw function for this drawable object
    *
    * @param g			Graphics2D instance
    * @param scale	the scale of a pixel
    * @param x			the x window position
    * @param y			the y window position
    */
  def draw(g: Graphics2D, scale: Int, x: Int, y: Int) = {
    g.setColor(color)
    g.fillRect(x - scale, y - scale, 3 * scale, 3 * scale)
  }
  
  
  /**	Damage the enemy and kill it if the damage was lethal
    *
    * @param dmg		The damage amount
    */
  def damage(dmg: Int) = {
    hp -= dmg
    if (hp <= 0) {
      die(this.value)
    }
  }
  
  
  /**	Moves the enemy by on tile if it has idled long enough.
   	* Attack the tower if it comes in contact with it.
   	* Otherwise increment the idleCount number.
   	* Is called every frame.
    */
  def move(): Unit = {
    if(idleCount >= speed) {
      val base = game.grid.get.base.get
      if(this.coord.inside(base.coord - Coord(base.size / 2, base.size / 2), base.coord + Coord(base.size / 2, base.size / 2))) {
        game.player.get.damage(damage)
        die(0)
      } else if(path.length > pathIndex) {
        idleCount = 0
        this.coord = path(pathIndex)
        pathIndex += 1
      }
    } else {
      idleCount += 1
    }
  }
  
  // Die and remove this enemy from the map
  private def die(reward: Int) = {
    game.grid.get.enemies -= this
    game.enemyDied(reward)
  }
}