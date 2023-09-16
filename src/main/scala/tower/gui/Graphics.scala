package tower

import scala.swing._
import scala.swing.event._
import scala.collection.mutable.Queue
import java.awt.BasicStroke
import java.awt.Color

/**	Creates a graphics instance
  *
  * @param width				width of the window
  * @param height				height of the window
  * @param cam					reference to a camera instance which is attached to a grid
  */
class Graphics(val width: Int, val height: Int, val cam: Camera) extends Component {  
  def grid = cam.grid
    
  // DEBUG
  private var drawCannotMove = false
  private var drawPlaceables = true
  private var drawPath = true
  private var drawFOW = true
  
  /**	Checks whether the the coordinate is in screen
    *
    * @param clip				length from the edges of the screen to clip
    * @param c					coordinate to be checked
    * @return	boolean		true if in screen otherwise false
    */
  def isInScreen(clip: Int, c: Coord) = c.inside(Coord(-clip, height + clip).toWorld, Coord(width + clip, -clip).toWorld)
  
  /* Draw pipeline (from top to bottom)
   * Map
   * Path
   * Enemies
   * Base
   * Towers
   * Lasers
   * Fog of War
   * Selection highlight
   * Mouse highlight
   * UI
   */
  override def paintComponent(g: Graphics2D): Unit = {
    // Draw map
    for (x <- 0 until width by cam.zoom; y <- 0 until height by cam.zoom) {
      val coord = Coord(x, y).toWorld
      val cell = grid.lift(coord)
      

      val color = if (cell.isDefined) {
        if (drawCannotMove && !cell.get.canPlace) {
          new Color(0, 0, 0)
        } else {
          cell.get.color
        }
      } else {
        new Color(0, 0, 0)
      }
      g.setColor(color)
      g.fillRect(x, y, cam.zoom, cam.zoom)
    }
    
    // Draw Path
    g.setColor(Color.RED)
    if (drawPath) {
      (0 until Main.game.enabledSpawners).foreach(index => {
        val p = grid.spawnPoints(index)
        var i = 0
        p.path.foreach(coord => {
          val pos = coord.toWindow
          if ((i - Main.currentFrame / 2) % 6 == 0) {
            g.fillRect(pos.x, pos.y, cam.zoom, cam.zoom)
          }
          i += 1
        })
      })
    }
    
    // Draw Enemies
    grid.enemies.foreach(e => {
      val pos = e.coord.toWindow
      e.draw(g, cam.zoom, pos.x, pos.y)
    })
    
    // Draw Base
    if (grid.base.isDefined) {
      val b = grid.base.get
        if (isInScreen(64, b.coord)) {
          val pos = b.coord.toWindow
          b.draw(g, cam.zoom, pos.x, pos.y)
        }
    }
    
    // Draw Towers
    if (drawPlaceables) {
      grid.towers.foreach(t => {
        if (isInScreen(64, t.coord)) {
          t.draw(g, cam.zoom, t.coord.x, t.coord.y)
        }
      })
    }

    // Draw lasers
    while (!grid.lasers.isEmpty) {
      val laser = grid.lasers.dequeue()
      g.setStroke(new BasicStroke(laser._1.size * 0.25f * cam.zoom))
      g.setColor(laser._1.color)
      val from = laser._1.coord.toWindow
      val to = laser._2.toWindow
      g.drawLine(from.x, from.y, to.x, to.y)
    }

    // Draw Fog of War
    if (Main.game.started && drawFOW) {
      for (x <- 0 until width by cam.zoom; y <- 0 until height by cam.zoom) {
        val coord = Coord(x, y).toWorld
        if ((coord - grid.base.get.coord).length > Main.game.radiusFOW) {
          g.setColor(new Color(0, 0, 0))
          g.fillRect(x, y, cam.zoom, cam.zoom)
        }
      }
    }
    
    // Draw selected radius
    g.setColor(new Color(255,255,255,95))
    if (InputHandler.selected.isDefined) {
      val t = InputHandler.selected.get
      val rSize = t.range.max(t.size)
      for (x <- t.coord.x - rSize until t.coord.x + rSize) {
        for (y <- t.coord.y - rSize until t.coord.y + rSize) {
          val coord = Coord(x, y)
          if ((t.coord - coord).length < rSize) {
            val winPos = Coord(x, y).toWindow
            g.fillRect(winPos.x, winPos.y, cam.zoom, cam.zoom)
          }
        }
      }
    }
    
    // Draw mouse highlight
    val hSize = InputHandler.highlightSize.max(InputHandler.coreHighlightSize) / 2
    for (x <- cam.mouseCoord.x - hSize until cam.mouseCoord.x + hSize) {
      for (y <- cam.mouseCoord.y - hSize until cam.mouseCoord.y + hSize) {
        val coord = Coord(x, y)
        val diff = (cam.mouseCoord - coord).length
        if (InputHandler.rectHighlight || diff < hSize) {
          if (diff < InputHandler.coreHighlightSize / 2) {
            g.setColor(new Color(255,255,255,200))
          } else {
            g.setColor(new Color(255,255,255,127))
          }
          
          val winPos = coord.toWindow
          g.fillRect(winPos.x, winPos.y, cam.zoom, cam.zoom)
        }
      }
    }
    
    // Draw UI
    UI.draw(g)
  }
  
  
  
  reactions += InputHandler.getControls(cam)
  
  listenTo(mouse.clicks, mouse.moves, mouse.wheel, keys)
  focusable = true
}