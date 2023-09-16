package tower

import java.io.FileReader
import java.io.File
import java.io.BufferedReader
import java.awt.Color
import scala.collection.mutable.Buffer
import scala.collection.mutable.Map
import scala.collection.mutable.Queue
import scala.util.Random

object Parser {
  /**	Parses the config file
    *
    * @param path		path of the file
    */
  def readConfig(path: String): GameSettings = {
    val reader = new FileReader(new File(path))
    val lineReader = new BufferedReader(reader)
    
    var windowWidth: Option[Int] = None
    var windowHeight: Option[Int] = None
    var seed: Option[Int] = None
    var mapWidth: Option[Int] = None
    var mapHeight: Option[Int] = None
    var radiusFOW: Option[Int] = None
    var startMoney: Option[Int] = None
    var startLives: Option[Int] = None
    
    var currentLine: String = ""
    while ({currentLine = lineReader.readLine(); currentLine != null}) {
      currentLine = currentLine.trim()
      
      val splitted = currentLine.split(":")
      if (splitted.length == 2) {
        val field = splitted(0).trim
        val value = splitted(1).trim
        if (field == "WINDOW_WIDTH") {
          windowWidth = Some(value.toInt)
        } else if (field == "WINDOW_HEIGHT") {
          windowHeight = Some(value.toInt)
        } else if (field == "SEED") {
          if (value == "RANDOM") {
            seed = Some(Random.nextInt)
          } else {
            seed = Some(value.toInt)
          }
        } else if (field == "MAP_WIDTH") {
          mapWidth = Some(value.toInt)
        } else if (field == "MAP_HEIGHT") {
          mapHeight = Some(value.toInt)
        } else if (field == "FOG_OF_WAR") {
          radiusFOW = Some(value.toInt)
        } else if (field == "START_MONEY") {
          startMoney = Some(value.toInt)
        } else if (field == "START_LIVES") {
          startLives = Some(value.toInt)
        }
      }
    }
    if (windowWidth.isDefined && windowHeight.isDefined && seed.isDefined && mapWidth.isDefined && mapHeight.isDefined && radiusFOW.isDefined && startMoney.isDefined && startLives.isDefined) {
      GameSettings(windowWidth.get, windowHeight.get, seed.get, mapWidth.get, mapHeight.get, radiusFOW.get, startMoney.get, startLives.get)
    } else {
      throw new Exception("Invalid config file")
    }
  }
  
  /**	Parses the tower file
    *
    * @param path		path of the file
    */
  def readTowers(path: String): Seq[TowerInfo] = {
    val reader = new FileReader(new File(path))
    val lineReader = new BufferedReader(reader)
    val towers = Buffer[TowerInfo]()
    
    var currentTower: TowerInfo = null
    var currentLine: String = ""
    while ({currentLine = lineReader.readLine(); currentLine != null}) {
      currentLine = currentLine.trim()

      if (!currentLine.startsWith("//")) {
        if (currentLine.startsWith("TOWER")) {
          currentTower = new TowerInfo
        } else if (currentLine.startsWith("END")) {
          if (currentTower.isValid) {
            towers += currentTower
            currentTower = null
          } else {
            println("Invalid Tower")
          }
        } else {
          val value = currentLine.drop(4)
          currentLine.take(3) match {
            case "NAM" => currentTower.name = Some(value)
            case "CLR" => currentTower.color = {
              val rgb = value.split(" ")
              val color = new Color(rgb(0).toInt, rgb(1).toInt, rgb(2).toInt)
              Some(color)
            }
            case "PRC" => currentTower.price = Some(value.toInt)
            case "SIZ" => currentTower.radius = Some(value.toInt)
            case "RNG" => currentTower.range = Some(value.toInt)
            case "DMG" => currentTower.damage = Some(value.toInt)
            case "FIR" => currentTower.firerate = Some(value.toInt)
            case _ => Unit
          }
        }
      }
    }
    
    lineReader.close()
    reader.close()
    towers.toSeq
  }
  
  /**	Parses the enemy file
    *
    * @param path		path of the file
    */
  def readEnemies(path: String): Map[String, EnemyInfo] = {
    val reader = new FileReader(new File(path))
    val lineReader = new BufferedReader(reader)
    val enemies = Map[String, EnemyInfo]()
    
    var currentEnemy: EnemyInfo = null
    var currentLine: String = ""
    while ({currentLine = lineReader.readLine(); currentLine != null}) {
      currentLine = currentLine.trim()
      
      if (!currentLine.startsWith("//")) {
        if (currentLine.startsWith("ENEMY")) {
          currentEnemy = new EnemyInfo
        } else if (currentLine.startsWith("END")) {
          if (currentEnemy.isValid) {
            enemies += currentEnemy.name.get.toLowerCase -> currentEnemy
            currentEnemy = null
          } else {
            println("Invalid Enemy")
          }
        } else {
          val value = currentLine.drop(4)
          currentLine.take(3) match {
            case "NAM" => currentEnemy.name = Some(value)
            case "CLR" => currentEnemy.color = {
              val rgb = value.split(" ")
              val color = new Color(rgb(0).toInt, rgb(1).toInt, rgb(2).toInt)
              Some(color)
            }
            case "SPD" => currentEnemy.speed = Some(value.toInt)
            case "MHP" => currentEnemy.maxhp = Some(value.toInt)
            case "DMG" => currentEnemy.damage = Some(value.toInt)
            case "VAL" => currentEnemy.value = Some(value.toInt)
            case _ => Unit
          }
        }
      }
    }
    
    lineReader.close()
    reader.close()
    enemies
  }
  
  /**	Parses the wave file
    *
    * @param path		path of the file
    */
  def readWaves(path: String, enemies: Map[String, EnemyInfo]): Queue[Wave] = {
    val reader = new FileReader(new File(path))
    val lineReader = new BufferedReader(reader)
    val waves = Queue[Wave]()
    
    var currentWave: Wave = null
    var currentSpawner: SpawnerContent = null
    var currentLine: String = ""
    while ({currentLine = lineReader.readLine(); currentLine != null}) {
      currentLine = currentLine.trim()
      
      if (!currentLine.startsWith("//")) {
        if (currentLine.startsWith("WAVE")) {
          currentWave = new Wave
          currentWave.value = currentLine.drop(5).toInt
          waves.enqueue(currentWave)
        } else if (currentLine.startsWith("SPAWNER")) {
          currentSpawner = new SpawnerContent
          currentWave.content = currentWave.content :+ currentSpawner
        } else if (!currentLine.isEmpty) {
          val split = currentLine.split(" ")
          if (split.length >= 2) {
            val name = split.dropRight(1).mkString(" ")
            val amount = split.last.toInt
            
            if (enemies.contains(name)) {
              val enemyContent = new EnemyContent(enemies(name), amount)
              currentSpawner.content.enqueue(enemyContent)
            } else {
              println("Enemy " + name + " does not exist")
            }
          }
        }
      }
    }
    
    lineReader.close()
    reader.close()
    waves
  }
}