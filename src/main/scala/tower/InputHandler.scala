package tower

import scala.swing.event.MouseMoved
import scala.swing.event.MouseClicked
import scala.swing.event.MouseWheelMoved
import scala.swing.event.KeyPressed
import scala.swing.event.Event

object InputHandler {
  var clickListeners: Map[Int, (Coord, Coord)] = collection.immutable.Map()
  var hoverListeners: Map[Int, ((Boolean, Boolean), (Coord, Coord))] = collection.immutable.Map()
  
  // Initial clickAction should be placing the base and starting the game
  var clickAction: (Coord, Int) => Unit = ClickAction.placeBase
  
  // Highlight options
  var highlightSize: Int = 8
  var coreHighlightSize: Int = 0
  var rectHighlight: Boolean = true
  
  var selectedMouse: Option[Int] = None
  var selected: Option[Tower] = None
  
  def addClickListener(e: UIElement): Unit = {
    clickListeners = clickListeners + (e.id -> (e.pos, e.pos + e.triggerSize.get))
  }
  
  def addHoverListener(e: UIElement, enter: Boolean, exit: Boolean): Unit = {
    hoverListeners = hoverListeners + (e.id -> ((enter, exit), (e.pos, e.pos + e.triggerSize.get)))
  }
  
  def resetClickAction() = {
    clickAction = ClickAction.select
    highlightSize = 4
    coreHighlightSize = 0
    rectHighlight = false
  }
  
  def getControls(cam: Camera): PartialFunction[Event, Unit] = {
    case MouseMoved(c, point, mods) => {
      cam.mousePos = Coord(point.x, point.y)
      this.mouseMove(cam.mousePos)
    }
    case MouseClicked(c, point, mods, clicks, triggersPopup) => {
      this.mouseClick(Coord(point.x, point.y), mods)      
    }
    case MouseWheelMoved(c, point, mods, rotation) => {
      cam.zoom = (cam.zoom - rotation).max(4).min(8)
    }
    case KeyPressed(c, key, mods, loc) => {
      if (Main.game.started) {
        val zSpeed = Main.width / (cam.zoom * 64) * (if (mods == 64) 4 else 1)
              
        key.toString match {
          case "W" | "Up" => cam.point += Coord(0, zSpeed)
          case "S" | "Down" => cam.point -= Coord(0, zSpeed)
          case "A" | "Left" => cam.point -= Coord(zSpeed, 0)
          case "D" | "Right" => cam.point += Coord(zSpeed, 0)
          case "Q" => Main.game.startWave()
          case "Space" => Main.game.pause()
          case _ => Unit
        }
      }
    }
  }
  
  private def mouseMove(c: Coord) = {
    hoverListeners.foreach(p => {
      val (id, ((enter, exit), (pos1, pos2))) = p
      
      val e = UI.lift(id)
      if (e.isEmpty) hoverListeners = hoverListeners - id
      else {
        val in = c.inside(pos1, pos2)
        if (!in && e.get.entered) {
          if (exit) e.get.exit()
          e.get.entered = false
        } else if (in && !e.get.entered) {
          if (enter) e.get.enter()
          e.get.entered = true
        }
      }
    })
  }
  
  private def mouseClick(c: Coord, mods: Int) = {    
    val grid = Main.game.grid.get
    
    // Do UI layer first
    var actionUI = false
    clickListeners.foreach(p => {
      val (id, (pos1, pos2)) = p
      
      if (c.inside(pos1, pos2)) {
        val e = UI.lift(id)
        
        if (e.isDefined) {
          actionUI = true
          e.get.click()
        }
        else clickListeners = clickListeners - id
      }
    })
    
    // Do action on map if didn't interact with UI
    if (!actionUI) this.clickAction(c, mods)
  }
}

object ClickAction {
  def grid = Main.game.grid.get
  
  def empty: (Coord, Int) => Unit = (_: Coord, _: Int) => Unit
  
  def select = (c: Coord, _: Int) => {
    val coord = c.toWorld
    val t = grid.towers.find(t => (t.coord - coord).length < t.size)
    InputHandler.selected = t
  }
  
  def placeBase = (c: Coord, _: Int) => {
    val coord = c.toWorld
    if (canBuild(coord, 4, true) == 0) {
      InputHandler.resetClickAction()
      Main.game.startGame(coord)
    } else {
      Main.game.setWarning("Can't place there!", 30)
    }
  }
  
  def buyTower(info: TowerInfo) = (c: Coord, m: Int) => {
    if (m == 256) {
      InputHandler.resetClickAction()
    } else if (Main.game.player.get.money >= info.price.get) {
      val tower = new Tower(Main.game, info)
      val coord = c.toWorld
      val code = canBuild(coord, info.radius.get, false)
      if (code == 0) {
        grid.placeTower(tower, coord)
        
        if (grid.spawnPoints.forall(!_.path.isEmpty)) {
          Main.game.player.get.spendMoney(tower.price)
          InputHandler.selected = Some(tower)
          InputHandler.resetClickAction()
        } else {
          grid.removeTower(tower)
          Main.game.setWarning("Can't block only path!", 30)
        }
      } else {
        code match {
          case 1 => Main.game.setWarning("Can't place on path during a wave!", 30)
          case 2 => Main.game.setWarning("Can't place in the fog of war!", 30)
          case 3 => Main.game.setWarning("Can't place there!", 30)
        }
      }
    } else {
      Main.game.setWarning("Can't afford!", 30)
    }
  }
  
  /**	Checks whether you can build in that location
    *
    * @param coord				location of the placeable
    * @param radius				radius of the placeable
    * @param isSquare			whether the placeable is a square
    * @return	errorcode		refer to the table below
    * Code | Description
    * 	0: 		Success
    * 	1: 		Placing on path during a wave
    * 	2: 		Placing in the fog of war
    * 	3: 		Placing on unplaceable terrain
    */
  private def canBuild(coord: Coord, radius: Int, isSquare: Boolean): Int = {
    var res = 0
    
    // Disallow placing on path in a wave
    if (Main.game.currentWave.isDefined && !grid.spawnPoints.filter(s => !s.path.forall(c => (c - coord).length >= radius + math.sqrt(2))).isEmpty) {
      res = 1
    }
    
    // Disallow placing in the Fog of War
    if (res == 0 && !isSquare && (coord - Main.game.grid.get.base.get.coord).length > Main.game.radiusFOW - radius) {
      res = 2
    }
    
    if (res == 0) {
      for (x <- coord.x - radius until coord.x + radius; y <- coord.y - radius until coord.y + radius) {
        if (res == 0) {
          if (isSquare || (Coord(x,y) - coord).length < radius) {
            val cell = grid.lift(x)(y)
            if (!cell.isDefined || !cell.get.canPlace) {
              res = 3
            }
          }
        }
      }
    }
    res
  }
}