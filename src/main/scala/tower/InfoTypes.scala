package tower

import scala.collection.mutable.Queue
import java.awt.Color

case class GameSettings(windowWidth: Int, windowHeight: Int, seed: Int, mapWidth: Int, mapHeight: Int, radiusFOW: Int, startMoney: Int, startLives: Int)

// Holds information about the wave
class Wave {
  var content: Seq[SpawnerContent] = Seq()
  var value: Int = 0
}

// Holds orders for a spawner
class SpawnerContent {
  var content: Queue[EnemyContent] = Queue()
  var delay: Int = 30
}

// Holds information about the specific spawnerorder
class EnemyContent(val enemy: EnemyInfo, val count: Int)

// Used to hold tower info and instantiating Tower instances
class TowerInfo {
    var name: Option[String] = None
    var color: Option[Color] = None
    var price: Option[Int] = None
    var radius: Option[Int] = None
    var range: Option[Int] = None
    var damage: Option[Int] = None
    var firerate: Option[Int] = None
    
    def isValid = {
      name.isDefined && color.isDefined && price.isDefined &&
      radius.isDefined && radius.isDefined && range.isDefined &&
      damage.isDefined && firerate.isDefined
    }
}

// Used to hold enemy info and instantiating Enemy instances
class EnemyInfo {
  var name: Option[String] = None
  var color: Option[Color] = None
  var speed: Option[Int] = None
  var maxhp: Option[Int] = None
  var damage: Option[Int] = None
  var value: Option[Int] = None
    
  def isValid = {
    name.isDefined && color.isDefined && speed.isDefined &&
    maxhp.isDefined && damage.isDefined && value.isDefined
  }
}