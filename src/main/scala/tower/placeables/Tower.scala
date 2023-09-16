package tower

import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.image.ColorModel
import java.io.File
import java.awt.Graphics2D
import java.awt.image.IndexColorModel
import java.awt.Color


/**	Creates a tower instance
  *
  * @param game				reference the the game instance this belongs to
  * @param name				name of the tower
  * @param radius			radius of the tower
  * @param price			price of the tower
  * @param range			the tower's attack range
  * @param color			color of the tower
  * @param damage			damage the tower deals to enemies
  * @param fireRate		fire rate of the tower. Lower is faster (the value is literally the idle time between shots)
  */
class Tower( val game: Game,
    val name: String, radius: Int, val color: Color,
    val price: Int, val range: Int, 
    val damage: Int, val fireRate: Int
) extends Placeable {
  
  val square = false
  val size = radius
  
  // 0 = closest, 1 = furthest, 2 = highest hp, 3 = lowest hp
  var targetingMode = 0
  
  def this(game: Game, i: TowerInfo) = this(game, i.name.get, i.radius.get, i.color.get, i.price.get, i.range.get, i.damage.get, i.firerate.get)
  
  private var idleCount = 0
  
  /**	Draw function for this drawable object
    *
    * @param g			Graphics2D instance
    * @param scale	the scale of a pixel
    * @param x			the x window position
    * @param y			the y window position
    */
  def draw(g: Graphics2D, scale: Int, x: Int, y: Int) = {
    for(x0 <- x - radius to x + radius; y0 <- y - radius to y + radius) {
      val diff = (Coord(x0, y0) - this.coord).length
      if (diff.toInt <= this.radius - 1) {
        val pos = Coord(x0, y0).toWindow
        g.setColor(if (diff.toInt == this.radius - 1) Color.BLACK else color)
        g.fillRect(pos.x, pos.y, scale, scale)
      }
    }
  }
  
  
  /**	Shoots an enemy idled long enough and in range.
   	* The enemy selected dependsd on targeting mode.
   	* Otherwise increment the idleCount number.
   	* Is called every frame.
    */
  def attack() = {    
    val grid = game.grid.get
    
    if(idleCount >= fireRate) {
      var target: Option[Enemy] = getTarget
      
      if(target.isDefined) {
        shoot(target.get)
        idleCount = 0
      }
    } else {
      idleCount += 1
    }
  }
  
  // Sells the tower for half the money
  def sell() = {
    game.player.get.money += this.price / 2
    game.grid.get.removeTower(this)
  }
  
  // Goes to the next targeting mode
  def nextMode() = targetingMode = (targetingMode + 1) % 4
  
  // PRIVATE
  
  // Compares the two targets and selectes them depending on the targeting mode
  private def compareTarget(target: Enemy, other: Enemy): Boolean = {
    val grid = game.grid.get
    
    def closer = (target.coord - grid.base.get.coord).length > (other.coord - grid.base.get.coord).length
    
    // refer targetingMode for info
    targetingMode match {
      case 0 => closer
      case 1 => (target.coord - grid.base.get.coord).length < (other.coord - grid.base.get.coord).length
      case 2 => target.hp < other.hp || (target.hp == other.hp && closer)
      case 3 => target.hp > other.hp || (target.hp == other.hp && closer)
    }
  }
  
  // Gets a target in range
  private def getTarget: Option[Enemy] = {
    val grid = game.grid.get
    var target: Option[Enemy] = None
    
    grid.enemies.foreach(e => {
      if (
        (range == 0 || (e.coord - this.coord).length <= range)
        && (e.coord - game.grid.get.base.get.coord).length < game.radiusFOW
      ) {
        if (target.isEmpty || compareTarget(target.get, e)) {
          target = Some(e)
        }
      }
    })
    
    target
  }
  
  // Shoot an enemy
  private def shoot(target: Enemy) = {
    target.damage(damage)
    game.grid.get.lasers.enqueue((this, target.coord))
  }
}
