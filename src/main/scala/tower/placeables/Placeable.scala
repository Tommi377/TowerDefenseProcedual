package tower

import java.awt.Graphics2D
import javax.imageio.ImageIO
import java.io.File

// Contains all the fields that an object, that can be placed on the grid, should have
trait Placeable {
  val name: String
  val square: Boolean
  val size: Int
  var coord = Coord(-1, -1)
  def draw(g: Graphics2D, scale: Int, x: Int, y: Int)
}

// Base of the player. It will result in a game over if the player fails to protect this.
class Base extends Placeable {
  val square = true
  val size = 8
  val name = "Base"
  val sprite = ImageIO.read(new File("images/base.png"))
  
  def draw(g: Graphics2D, scale: Int, x: Int, y: Int) = {
    g.drawImage(sprite, x - (size / 2) * scale, y - (size / 2) * scale + scale, scale * size, scale * size, null)
  }
}