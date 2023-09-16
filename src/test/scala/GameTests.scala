import org.scalatest.FlatSpec
import tower.Game

class GameTests extends FlatSpec  {
  var game: Game = new Game(0)
  
  "A Game" should "create the necessary objects in initGame" in {
    game.initGame()
    assert(game.player.isDefined)
    assert(game.grid.isDefined)
    assert(game.camera.isDefined)
  }
  
  
  it should "start the game and init the UI when startGame is called" in {
    game.startGame()
    assert(game.grid.get.base.isDefined)
    assert(game.waves.head.content.length == game.enabledSpawners)
  }
  
  it should "start a wave when startWave is called" in {
    val prevWave = game.wave
    game.startWave()
    assert(game.wave == prevWave + 1)
    assert(game.currentWave.isDefined)
  }

  it should "create a spawner when createSpawner(s) is called" in {
    var count = game.grid.get.spawnPoints.length
    game.createSpawner()
    assert(game.grid.get.spawnPoints.length == count + 1)
    
    val amount = 2
    count = game.grid.get.spawnPoints.length
    game.createSpawners(amount)
    assert(game.grid.get.spawnPoints.length == count + amount)
  }
}