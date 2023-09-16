import org.scalatest.FlatSpec
import tower.Game
import tower.Tower
import java.awt.Color
import tower.Enemy
import tower.Coord

class TowerTests extends FlatSpec {
  val game = new Game()
  game.initGame()
  val grid = game.grid.get
  
  val tower = new Tower(game, "tower", 2, Color.RED, 100, 60, 2, 0)
  val coord = Coord(game.mapWidth / 2, game.mapHeight / 2)
  
  val enemy1 = new Enemy(game, 1, 10, 2, Color.BLUE, 10)
  val enemy2 = new Enemy(game, 2, 10, 2, Color.BLUE, 10)
  val highHp = new Enemy(game, 3, 50, 2, Color.BLUE, 10)
  val lowHp = new Enemy(game, 4, 5, 2, Color.BLUE, 10)
  
  val closestCoord = Coord(game.mapWidth / 2 - 5, game.mapHeight / 2 - 5)
  val farthestCoord = Coord(game.mapWidth / 2 - 40, game.mapHeight / 2 - 40)
  val middleCoord1 = Coord(game.mapWidth / 2 - 20, game.mapHeight / 2 - 20)
  val middleCoord2 = Coord(game.mapWidth / 2 + 20, game.mapHeight / 2 + 20)
  
  grid.placeBase(coord)
  grid.placeTower(tower, coord)
  grid.spawn(enemy1, closestCoord)
  grid.spawn(enemy2, farthestCoord)
  grid.spawn(highHp, middleCoord1)
  grid.spawn(lowHp, middleCoord2)
  
  "An Tower" should "shoot the closest enemy on mode 1" in {
    assert(enemy1.hp == enemy1.maxhp)
    assert(tower.targetingMode == 0)
    
    tower.attack()
    assert(enemy1.hp != enemy1.maxhp)
    
  }
  
  it should "shoot the farthest enemy on mode 2" in {
    assert(enemy2.hp == enemy2.maxhp)
    tower.nextMode()
    assert(tower.targetingMode == 1)
    
    tower.attack()
    assert(enemy2.hp != enemy2.maxhp)
  }
  
  it should "shoot the enemy with highest hp on mode 3" in {
    assert(highHp.hp == highHp.maxhp)
    tower.nextMode()
    assert(tower.targetingMode == 2)
    
    tower.attack()
    assert(highHp.hp != highHp.maxhp)
  }
  
  it should "shoot the enemy with lowest hp on mode 4" in {
    assert(lowHp.hp == lowHp.maxhp)
    tower.nextMode()
    assert(tower.targetingMode == 3)
    
    tower.attack()
    assert(lowHp.hp != lowHp.maxhp)
  }
  
  it should "sell the tower for half the price and remove it from the map" in {
    val prevMoney = game.player.get.money
    
    assert(grid.towers.contains(tower))
    tower.sell()
    assert(!grid.towers.contains(tower))
    assert(game.player.get.money == prevMoney + tower.price / 2)
  }
}