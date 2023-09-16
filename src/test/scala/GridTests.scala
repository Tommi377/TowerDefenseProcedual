import org.scalatest.FlatSpec
import tower.Game
import tower.Grid
import tower.Tower
import java.awt.Color
import tower.Coord

class GridTests extends FlatSpec {
  val game = new Game()
  val grid = new Grid(game, game.mapWidth, game.mapHeight)
  
  val tower1 = new Tower(game, "tower", 4, Color.RED, 100, 2, 2, 2)
  val tower2 = new Tower(game, "tower", 4, Color.RED, 100, 2, 2, 2)
  val tower3 = new Tower(game, "tower", 4, Color.RED, 100, 2, 2, 2)
  val baseCoord = Coord(250, 250)
  
  "A Grid" should "generate the map when generateMap is called" in {
    grid.generateMap()
    assert(grid.lift(0)(0).isDefined)
  }
  
  it should "place a base in the specifield location when placeBase is called" in {

    
    assert(grid.base.isEmpty)
    grid.placeBase(baseCoord)
    assert(grid.base.isDefined)
    assert(grid.base.get.coord == baseCoord)
  }
  
  it should "create a tower when placeTower is called" in {
    var towerCount = grid.towers.length
    val coord1 = Coord(game.mapWidth / 2, game.mapHeight / 2)
    val coord2 = Coord(game.mapWidth / 2 + 8, game.mapHeight / 2 + 8)
    val coord3 = Coord(game.mapWidth / 2 - 8, game.mapHeight / 2 - 8)
    
    grid.placeTower(tower1, coord1)
    assert(tower1.coord == coord1)
    assert(grid.towers.length == towerCount + 1)
    
    towerCount = grid.towers.length
    
    grid.placeTower(tower2, coord2)
    grid.placeTower(tower3, coord3)
    assert(tower2.coord == coord2)
    assert(tower3.coord == coord3)
    assert(grid.towers.length == towerCount + 2)
  }
  
  it should "remove a tower when removeTower is called" in {
    var towerCount = grid.towers.length
    
    grid.removeTower(tower1)
    assert(grid.towers.length == towerCount - 1)
  }
}