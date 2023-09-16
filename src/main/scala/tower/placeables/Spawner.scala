package tower

import java.awt.Graphics2D
import javax.imageio.ImageIO
import java.io.File
import scala.collection.mutable.Buffer
import scala.collection.mutable.Queue


/**	Creates a spawner instance
  *
  * @param game				reference the the game instance this belongs to
  * @param coord			location of this spawner in world coordinates
  */
class Spawner(val game: Game, val coord: Coord) {
  var path: Buffer[Coord] = Buffer()
  private val pathSize = 3
  
  // Wave related
  private var queue = Queue[EnemyContent]()
  private var currentOrder: Option[EnemyContent] = None
  private var currentSpawnCount = 0
  private var orderDelay = 0
  
  private var idleCount = 0
  
  // Instantiate and spawn and enemy on the grid
  def spawn(info: EnemyInfo): Enemy = {
    spawn(new Enemy(game, info))
  }
  
  // Spawn an already instantiated enemy
  def spawn(enemy: Enemy): Enemy = {
    enemy.path = this.path
    game.grid.get.spawn(enemy, this.coord)
    enemy
  }
  
  /**	generate a path from the spawner to the target
    *
    * @param target			position of the target in world coordinates
    */
  def updatePath(target: Coord) = {
    val option = contructPath(target)
    path = if (option.isEmpty) {
      println("path not found")
      Buffer()
    } else {
      option.get
    }
  }
  
  // Checks whether this spawner is done with the current the wave
  def done = this.queue.isEmpty && this.currentOrder.isEmpty
  
  // Checks whether the current path is legal or not
  def isLegalPath = this.path.forall(checkNode)
  
  /**	Spawns an enemy if it has idled long enough.
   	* Goes through all the queued up orders until nothing left.
   	* Otherwise increment the idleCount number.
   	* Is called every frame.
    */
  def update() = {
    if(currentOrder.isDefined) {
      if(currentSpawnCount < currentOrder.get.count) {
        if(idleCount >= (currentOrder.get.enemy.speed.get + 1) * 4) {
          idleCount = 0
          currentSpawnCount += 1
          spawn(currentOrder.get.enemy)
        } else {
          idleCount += 1
        }
      } else {
        if(idleCount >= orderDelay) {
          idleCount = 0
          nextOrder()
        } else {
          idleCount += 1
        }
      }
    }
  }
  
  // Initializes the spawner for the wave
  def startWave(order: SpawnerContent) = {
    queue = order.content
    orderDelay = order.delay
    nextOrder()
  }
  
  // PRIVATE
  
  /** The A* algorithm for constructing a path from a to b
    *
    * @param target			position of the target in world coordinates
    */
  private def contructPath(target: Coord) = {
    def len(node: Coord) = (target - node).length
    
    val maxIterations = 300000
    var iterations = 0
    var ret: Option[Buffer[Coord]] = None
    
    val open = Buffer(this.coord)
    val closed = Buffer[Coord]()
    val cameFrom = collection.mutable.Map[Coord, Coord]()
    
    val gScore = collection.mutable.Map[Coord, Double](this.coord -> 0)
    val fScore = collection.mutable.Map[Coord, Double](this.coord -> len(this.coord))
    
    var done = false
    while (!open.isEmpty && !done) {
      var current = open.minBy(fScore(_))
      if (iterations >= maxIterations) {
        done = true
      } else if (current != target) {
        closed += current
        open -= current
        
        for (x <- current.x - 1 to current.x + 1) {
          for (y <- current.y - 1 to current.y + 1) {
            iterations += 1
            val neighbor = Coord(x, y)
            if (
                neighbor != current && checkNode(neighbor) &&
                (game.grid.get.base.get.coord - neighbor).length < game.radiusFOW + 8
            ) {
              val gsCurrent = if (gScore.isDefinedAt(current)) gScore(current) else Double.MaxValue
              val gsNeigbor = if (gScore.isDefinedAt(neighbor)) gScore(neighbor) else Double.MaxValue
              
              val corner = (x - current.x).abs == 1 && (y - current.y).abs == 1
              val weight = if (corner) 1.4 else 1 // Longer distance if moving diagonally
              val gs = gsCurrent + weight
              
              if (gs < gsNeigbor) {
                cameFrom += neighbor -> current
                gScore += neighbor -> gs
                fScore += neighbor -> (gScore(neighbor) + len(neighbor))
                
                if (!closed.contains(neighbor)) {
                  open += neighbor
                }
              }
            }
          }
        }
        
      } else {
        // Path found
        val path = Buffer(current)
        while (cameFrom.contains(current)) {
          current = cameFrom(current)
          path += current
        }
        done = true
        ret = Some(path.reverse)
      }
    }
    
    ret
  }
  
  /** Tests whether the 3x3 node is a legal node
    *
    * @param node			position of the node to check in world coordinates
    * @return boolean true if node is legal
    */
  private def checkNode(node: Coord) = {
    node.x > 0 && node.y > 0 &&
    node.x < game.mapWidth - 1 && node.y < game.mapHeight - 1 &&
    game.grid.get(node + Coord(1, 1)).canMove &&
    game.grid.get(node + Coord(-1, -1)).canMove &&
    game.grid.get(node + Coord(-1, 1)).canMove &&
    game.grid.get(node + Coord(1, -1)).canMove
  }
  
  // Goes to the next order in the queue
  private def nextOrder() = {
    if (!queue.isEmpty) {
      currentOrder = Some(queue.dequeue())
      currentSpawnCount = 0
    } else {
      currentOrder = None
      currentSpawnCount = 0
      orderDelay = 0
    }
  }
}