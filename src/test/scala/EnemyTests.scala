import org.scalatest.FlatSpec
import tower.Game
import tower.Enemy
import java.awt.Color

class EnemyTests extends FlatSpec {
  val game = new Game()
  game.initGame()
  game.startGame()
  game.createSpawner()
  
  val value = 100
  val enemy = new Enemy(game, 2, 10, 2, Color.BLUE, value)
  val spawner = game.grid.get.spawnPoints(0)
  spawner.spawn(enemy)
  
  
  "An Enemy" should "have moved after some a few ticks" in {
    val initPos = enemy.coord
    for(i <- 0 until 6) {
      enemy.move()
    }
    
    assert(initPos != enemy.coord)
  }
  
  it should "take damage when calling damage method" in {
    val prevHp = enemy.hp
    val dmg = 5
    assert(enemy.hp == enemy.maxhp)
    enemy.damage(dmg)
    assert(enemy.hp == enemy.maxhp - dmg)
  }
  
  it should "die when taking lethal damage and reward player with money" in {
    val prevMoney = game.player.get.money
    val dmg = enemy.hp
    assert(game.grid.get.enemies.contains(enemy))
    enemy.damage(dmg)
    assert(!game.grid.get.enemies.contains(enemy))
    assert(game.player.get.money == prevMoney + enemy.value)
  }
}