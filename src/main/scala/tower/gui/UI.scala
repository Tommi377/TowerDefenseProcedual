package tower

import scala.swing._
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Font
import java.awt.FontMetrics

object UI {
  private var elements: Map[Int, UIElement] = collection.immutable.Map()
  
  // Register an UI element as a part of the UI
  def registerElement(e: UIElement): Int = {
    val id = Stream.from(0).find(!elements.contains(_)).get
    elements = elements + (id -> e)
    id
  }
  
  def apply(id: Int): UIElement = elements(id)
  
  def lift(id: Int): Option[UIElement] = elements.lift(id)
  
  def delete(id: Int) = elements = elements - id
  
  // Draw the UI
  def draw(g: Graphics2D) = UI.elements.values.foreach(_.draw(g))
}

object UIElement {
  /**	Generate a group of square buttons
    *
    * @param size								Size of the content of the button
    * @param border							Size of the border of the button
    * @param padding						Size of the padding of the button
    * @param pos								Position of the button in screen coordinates
    * @param amount							Amount of buttons
    * @param perRow							How many buttons are the per row
    * @param innerColor					Inner color of the button
    * @param outerColor					Outer color of the button
    * @param highlightColor			Color of the button when highlighted
    * @return	Seq[SquareButton]	Buttons that were generated
    */
  def generateSquareButtons(
      size: Int,
      border: Int,
      padding: Int,
      pos: Coord,
      amount: Int,
      perRow: Int,
      innerColor: Color,
      outerColor: Color,
      highlightColor: Color
  ): Seq[SquareButton] = {
    val totalSize = size + 2 * border + 2 * padding

    for (i <- 0 until amount) yield {
      val x = pos.x + totalSize * (i % perRow)
      val y = pos.y + totalSize * (i / perRow)
      
      new SquareButton(size, border, padding, Coord(x, y), innerColor, outerColor, highlightColor)
    }
  }
}

abstract class UIElement(val pos: Coord, val triggerSize: Option[Coord] = None, clickable: Boolean = false) {
  val id = UI.registerElement(this)
  var hidden = false
  var interactable = true
  var entered = false  // Only works if Hover Listener is added
  private var onClick: Int => Unit = (_) => Unit
  private var onEnter: Int => Unit = (_) => Unit
  private var onExit: Int => Unit = (_) => Unit
  
  def click() = if (interactable && !hidden) this.onClick(this.id)
  def enter() = if (!hidden) this.onEnter(this.id)
  def exit() = if (!hidden) this.onExit(this.id)
  
  def delete() = UI.delete(this.id)
  
  def addClickListener(f: (Int) => Unit) = {
    this.onClick = f
    InputHandler.addClickListener(this)
  }
  def addHoverListener(enter: Option[(Int) => Unit] = None, exit: Option[(Int) => Unit] = None) = {
    val en = enter.isDefined
    val ex = exit.isDefined
    if (en) this.onEnter = enter.get
    if (ex) this.onExit = exit.get
    InputHandler.addHoverListener(this, en, ex)
  }
  
  def draw(g: Graphics2D): Unit = if (!hidden) this.drawFunction(g)
  
  def drawFunction(g: Graphics2D): Unit
}

/**	Generate a square button
  *
  * @param size								Size of the content of the button
  * @param border							Size of the border of the button
  * @param padding						Size of the padding of the button
  * @param pos								Position of the button in screen coordinates
  * @param innerColor					Inner color of the button
  * @param outerColor					Outer color of the button
  * @param highlightColor			Color of the button when highlighted
  */
class SquareButton(
      size: Int,
      border: Int,
      padding: Int,
      pos: Coord,
      innerColor: Color,
      outerColor: Color,
      highlightColor: Color,
      var icon: Option[BufferedImage] = None
  ) extends UIElement(pos + Coord(padding, padding), Some(Coord(size + 2 * border, size + 2 * border)), true) {
  var iconFunc = (g: Graphics2D, x: Int, y: Int, sz: Int) => {
    if (icon.isDefined) {
      g.drawImage(icon.get, x, y, sz, sz, null)
    }
  }
  
  this.addClickListener(_ => println("Clicked: " + this.id))
  this.addHoverListener()
    
  def drawFunction(g: Graphics2D): Unit = {
      g.setColor(outerColor)
      g.fillRect(pos.x + padding, pos.y + padding, size + 2 * border, size + 2 * border)
      g.setColor(if (entered) innerColor else highlightColor)
      g.fillRect(pos.x + border + padding, pos.y + border + padding, size, size)
      
      iconFunc(g, pos.x + border + padding, pos.y + border + padding, size)
      
      if (!interactable) {
        g.setColor(new Color(0, 0, 0, 127))
        g.fillRect(pos.x + padding, pos.y + padding, size + 2 * border, size + 2 * border)
      }
  }
}

/**	Generate a text component
  *
  * @param str						Text to be displayed
  * @param fontSize				Size of the font
  * @param pos						Position in screen coordinates
  * @param center					Whether the button is centered or not
  */
class TextComponent(str: =>String,  fontSize: Int, pos: Coord, center: Boolean = false) extends UIElement(pos) {
  val font = new Font("TimesRoman", Font.BOLD, fontSize)

  def drawFunction(g: Graphics2D) = {
    g.setFont(font)
    val x = if(!center) pos.x else {
      val fm: FontMetrics = g.getFontMetrics();
      (Main.width - fm.stringWidth(str)) / 2
    }
    
    g.setColor(Color.BLACK)
    g.drawString(str, x + 1 , pos.y + 1)
    g.setColor(Color.WHITE)
    g.drawString(str, x, pos.y)
  }
}


/**	Generate an info square
  *
  * @param title			Title of the info square
  * @param str				Content of the info square
  * @param pos				Position in screen coordinates
  * @param size				Size of the content of the info square
  * @param padding		Size of the padding of the info square
  */
class InfoSquare(title: =>String, str: =>Vector[String], pos: Coord, size: Coord, padding: Int) extends UIElement(pos) {
  val fontSize = 16
  val font = new Font("TimesRoman", Font.PLAIN, fontSize)
  
  val fontSizeTitle = 20
  val fontTitle = new Font("TimesRoman", Font.BOLD, fontSize)
  
  def drawFunction(g: Graphics2D) = {
    g.setColor(Color.GRAY)
    g.fillRect(pos.x, pos.y, size.x, size.y)
    g.setColor(Color.WHITE)
    
    g.setFont(fontTitle)
    g.drawString(title, pos.x + padding, pos.y + padding + fontSizeTitle)
    
    g.setFont(font)
    str.zipWithIndex.foreach(p => {
      val (s, i) = p
      g.drawString(s, pos.x + padding, pos.y + padding + fontSize * (i + 1) + fontSizeTitle)
    })
  }
}
      
 