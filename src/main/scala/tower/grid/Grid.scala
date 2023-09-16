package tower
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.collection.mutable.Queue

/**	Creates a player instance
  *
  * @param game			reference the the game instance this belongs to
  * @param xSize		map width
  * @param ySize		map height
  */
class Grid(val game: Game, val xSize: Int, val ySize: Int) {
  var grid: Array[Array[Cell]] = Array.ofDim(xSize, ySize)
  
  // Objects on the map
  var base: Option[Base] = None
  var towers: Buffer[Tower] = Buffer()
  var enemies: Buffer[Enemy] = Buffer()
  var spawnPoints: Buffer[Spawner] = Buffer()
  var lasers: Queue[(Tower, Coord)] = Queue()
  
  // Generation settings
  private val textureStep = 16
  private val minT = 0.0
  private val waterT = 0.25
  private val sandT = 0.4
  private val grassT = 0.8
  private val mountainT = 1.0
    
  // Get a cell from the grid
  def apply(x: Int)(y: Int) = grid(x)(y)
  def apply(c: Coord) = grid(c.x)(c.y)
  
  // Lift a cell from the grid
  def lift(x: Int)(y: Int): Option[Cell] = {
    try {
      Some(grid(x)(y))
    } catch {
      case _: IndexOutOfBoundsException => None
    }
  }
  def lift(c: Coord): Option[Cell] = lift(c.x)(c.y)
    
  /**	Generates a new map with the perlin noise algorithm
    * and replaces the grid with it
    *
    * @param octaves		octaves in the calculation
    * @param bias				bias in the calculation
    */
  def generateMap(octaves: Int = 6, bias: Double = 0.25) {
    def colorStep(value: Double, min: Double, max: Double, factor: Int): Int = (((value - min) / (max - min) * factor)).toInt / textureStep * textureStep
    val perlin = new Perlin(xSize, ySize, octaves, bias)
    perlin.calculatePerlin(game.seed)
    
    
    for(x <- 0 until xSize; y <- 0 until ySize) {
      grid(x)(y) = perlin(x)(y) match {
        case v if (v < waterT) => Cell.createWater(100 + colorStep(v, minT, waterT, 155))
        case v if (v < sandT) => Cell.createSand(255 - colorStep(v, waterT, sandT, 55))
        case v if (v < grassT) => Cell.createGrass(255 - colorStep(v, sandT, grassT, 100))
        case v => Cell.createMountain(100 - colorStep(v, grassT, mountainT, 64))
      }
    }
  }
    
    
  /**	Places the base on the specified location on the grid
    *
    * @param c		Position in world coordinates of the base
    */
  def placeBase(c: Coord) = {
    val base = new Base
    base.coord = c
    for (x <- c.x - base.size / 2 until c.x + base.size / 2)  {
      for (y <- c.y - base.size / 2 until c.y + base.size / 2) {
        val cell = this.lift(x)(y)
        if (cell.isDefined) {
          cell.get.canPlace = false
        }
      }
    }
    this.base = Some(base)
  }
  
    
  /**	Places a tower on the specified location on the grid
    * and modifies the affected area.
    *
    * @param t		Tower to be placed on the grid
    * @param c		Position in world coordinates of the tower
    */
  def placeTower(t: Tower, c: Coord) = {    
    t.coord = c
    towers += t
    for (x <- c.x - t.size until c.x + t.size)  {
      for (y <- c.y - t.size until c.y + t.size) {
        if ((Coord(x, y) - c).length < t.size) {
          val cell = this.lift(x)(y)
          if (cell.isDefined) {
            cell.get.canMove = false
            cell.get.canPlace = false
          }
        }
      }
    }
    
    spawnPoints.filter(!_.isLegalPath).foreach(_.updatePath(base.get.coord))
  }
  
    
  /**	Removes the tower from the grid
    *
    * @param t		Tower to be removed from the grid
    */
  def removeTower(t: Tower) = {
    towers -= t
    val c = t.coord
    for (x <- c.x - t.size - 1 until c.x + t.size + 1)  {
      for (y <- c.y - t.size - 1 until c.y + t.size + 1) {
        if ((Coord(x, y) - c).length < t.size + 1) {
          val cell = this.lift(x)(y)
          if (cell.isDefined) {
            cell.get.canMove = true
            cell.get.canPlace = true
          }
        }
      }      
    }
    
    spawnPoints.foreach(_.updatePath(base.get.coord))
  }
  
  /**	Creates a spawner on the specified location on the grid
    *
    * @param coord							Position in world coordinates of the spawner
    * @return Option[Spawner] 	Returns the spawner created wrapped in Some. None if spawner placement was illegal
    */
  def createSpawnPoint(coord: Coord): Option[Spawner] = {
    val spawn = new Spawner(game, coord)
    spawn.updatePath(base.get.coord)
    if (!spawn.path.isEmpty) {
      spawnPoints += spawn
      Some(spawn)
    } else None
  }
  
  def spawn(e: Enemy, c: Coord) = {
    e.coord = c
    enemies += e
  }
}

case class Coord(x: Int, y: Int) {
  def +(c: Coord): Coord = Coord(x + c.x, y + c.y)
  def -(c: Coord): Coord = Coord(x - c.x, y - c.y)
  def *(c: Coord): Coord = Coord(x * c.x, y * c.y)
  def /(c: Coord): Coord = Coord(x / c.x, y / c.y)
  
  def *(s: Int): Coord = Coord(x * s, y * s)
  
  def inside(pos1: Coord, pos2: Coord): Boolean = pos1.y <= this.y && pos1.x <= this.x && this.y <= pos2.y && this.x <= pos2.x
  
  def length: Double = math.sqrt(x * x + y * y)
  
  // Converts window coordinates to world coordinates
  def toWorld: Coord = {
    val cam = Main.game.camera.get
    val _x = x / cam.zoom - Main.width / (cam.zoom * 2) + cam.point.x
    val _y = - y / cam.zoom + Main.height / (cam.zoom * 2) + cam.point.y
    Coord(_x, _y)
  }
  
  // Converts world coordinates to window coordinates
  def toWindow: Coord = {
    val cam = Main.game.camera.get
    val _x = (x + Main.width / (cam.zoom * 2) - cam.point.x) * cam.zoom
    val _y = (- y + Main.height / (cam.zoom * 2) + cam.point.y) * cam.zoom
    Coord(_x, _y)
  }
  
  def toPair: (Int, Int) = (x, y)
  
  override def toString: String = s"($x, $y)"
}