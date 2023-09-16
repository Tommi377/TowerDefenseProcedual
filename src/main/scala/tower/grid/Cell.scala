package tower
import scala.swing.Color
import scala.collection.mutable.Buffer


/**	Creates a cell instance
  *
  * @param name				name of the cell
  * @param color			color of the cell
  * @param canPlace		whether you can place on this
  * @param canMove		whether you can move on this
  */
case class Cell(name: String, color: Color, var canPlace: Boolean, var canMove: Boolean = true)

object Cell {
  def createGrass(c: Int): Cell = new Cell("Grass", new Color(0, c, 0), true, true)
  def createWater(c: Int): Cell = new Cell("Water", new Color(0, c / 2, c), false, false)
  def createSand(c: Int): Cell = new Cell( "Sand", new Color(c, c, 0), true, true)
  def createMountain(c: Int): Cell = new Cell("Mountain", new Color(c, c, c), false, false)
}