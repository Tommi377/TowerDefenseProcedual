package tower
import scala.collection.mutable.Buffer
import scala.swing._

/**
 * An application that opens up a simple window.
 *
 * @param title 		a window title
 * @param width 		the window width in pixels
 * @param height  	the window height in pixels
 */
class Window(val width: Int, val height: Int, camera: Camera) extends SimpleSwingApplication {

  val graphics = new Graphics(width, height, camera)

  val top = new MainFrame {
    this.title = title
    this.preferredSize = new java.awt.Dimension(width, height)
    this.minimumSize = new java.awt.Dimension(width, height)
    this.maximumSize = new java.awt.Dimension(width, height)
    this.resizable = false
    
    contents = graphics
  }
}