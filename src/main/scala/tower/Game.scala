package tower

import java.awt.Color
import javax.imageio.ImageIO
import java.io.File
import scala.util.Random
import scala.collection.mutable.Queue
import java.awt.Graphics2D
import scala.collection.mutable.Map

/**	Creates a game instance
  *
  * @param seed						Seed for the random number generator
  * @param mapWidth				width of the map
  * @param mapHeight			height of the map
  * @param radiusFOW			radius of the fog of war
  * @param startingMoney	money at start
  * @param startingLives	lives at start
  */
class Game(val seed: Int = 0, val mapWidth: Int = 512, val mapHeight: Int = 512, val radiusFOW: Int = 86, val startingMoney: Int = 250, val startingLives: Int = 10) {
  
/**	Creates a game instance
  *
  * @param settings		GameSettings instance
  */
  def this(settings: GameSettings) = {
    this(settings.seed, settings.mapWidth, settings.mapHeight, settings.radiusFOW, settings.startMoney, settings.startLives)
  }
  
  val rng = new Random(seed)
  
  // UI related
  private val targetingDesc = Vector(
      "1: Target closest to base",
      "2: Target farthest from base",
      "3: Target the enemy with highest hp",
      "4: Target the enemy with lowest hp"
  )
  private val targetingIcon = Map(
      0 -> ImageIO.read(new File("images/closest.png")),
      1 -> ImageIO.read(new File("images/farthest.png")),
      2 -> ImageIO.read(new File("images/highest.png")),
      3 -> ImageIO.read(new File("images/lowest.png"))
  )
  
  private var infoTitle = ""
  private var infoContent = Vector[String]()
  
  private var warningContent = ""
  private var warningCounter = 0
  
  private var bigText: Option[TextComponent] = None
  private var pausedText: Option[TextComponent] = None
  private var towerButtons: Seq[SquareButton] = Seq()
  private var pauseButton: Option[SquareButton] = None
  private var nextWaveButton: Option[SquareButton] = None
  private var sellButton: Option[SquareButton] = None
  private var targetButton: Option[SquareButton] = None
  
  
  // Objects
  var player: Option[Player] = None
  var grid: Option[Grid] = None
  var camera: Option[Camera] = None
  var currentWave: Option[Wave] = None
  
  // State
  var wave = 0
  var enabledSpawners = 0
  private var reward = 0
  private var totalEnemies = 0
  private var paused = true
  
  var started = false
  var gameEnd = false
  
  val towers: Seq[TowerInfo]           = Parser.readTowers("config/Towers.txt")
  val enemies: Map[String, EnemyInfo]  = Parser.readEnemies("config/Enemies.txt")
  val waves: Queue[Wave]               = Parser.readWaves("config/Waves.txt", enemies) 
  
  // Initializes the game with 
  def initGame() = {
    player = Some(new Player(this, startingMoney, startingLives))
    grid = Some(new Grid(this, mapWidth, mapHeight))
    grid.get.generateMap()

    camera = Some(new Camera(grid.get))
    
    // Only UI element that needs to exist in init phase
    val bigTxt = new TextComponent(warningContent, 30, Coord(0, Main.height / 2 - 32), true)
    bigTxt.hidden = true
    bigText = Some(bigTxt)
    
    this.setDefaultInfo()
  }
    
  /**	Places the base in the location and starts the game
    *
    * @param coord		location on the base in world coordinates
    */
  def startGame(coord: Coord = Coord(mapWidth / 2 + 50, mapHeight / 2 + 50)): Unit = {
    started = true
    grid.get.placeBase(coord)
    
    enabledSpawners = waves.head.content.length
    createSpawners(enabledSpawners)
    
    this.initUI()
  }
  
    
  // Advances the game by one game tick
  // Should be called every frame
  def update() = {
    if (this.started && !this.gameEnd) {
      if (player.get.lives <= 0) {
        new TextComponent("GAME OVER", 40, Coord(0, Main.height / 4), true)
        gameEnd = true
      }
      
      if (!paused) {
        if (currentWave.isDefined) {
          grid.get.enemies.clone.foreach(_.move())
          grid.get.towers.foreach(_.attack())
          grid.get.spawnPoints.foreach(_.update())
        }
      }
      
      if (currentWave.isDefined && waveEnded) {
        player.get.getCredit(currentWave.get.value)
        currentWave = None
        if (waves.isEmpty) {
          new TextComponent("YOU WON", 40, Coord(0, Main.height / 4), true)
          gameEnd = true
        } else {
          println("WAVE " + wave + " ended!")
          enabledSpawners = waves.head.content.length
          if (grid.get.spawnPoints.length < enabledSpawners) {
            createSpawners(enabledSpawners - grid.get.spawnPoints.length)
          }
          
          nextWaveButton.get.interactable = true
        }
      }
      updateUI()
    }
    
    if (!bigText.get.hidden) {
      if (warningCounter <= 0) {
        bigText.get.hidden = true
      } else {
        warningCounter -= 1
      }
    }
  }
  
  // Starts the next wave in the waves queue
  def startWave() = {
    if (!gameEnd && this.currentWave.isEmpty) {
      wave += 1
      setWarning("Starting wave " + wave, 60)
      currentWave = Some(waves.dequeue())
      totalEnemies = currentWave.get.content.foldLeft(0)(_ + _.content.foldLeft(0)(_ + _.count))
      reward = currentWave.get.value
      
      enabledSpawners = currentWave.get.content.length
      
      currentWave.get.content.zipWithIndex.foreach(pair => grid.get.spawnPoints(pair._2).startWave(pair._1))
      this.setDefaultInfo()
    }
  }
  
  // 
  /**	Called when an enemy dies
    *
    * @param reward		money to be credited
    */
  def enemyDied(reward: Int) = {
    totalEnemies -= 1
    player.get.getCredit(reward)
    if (infoContent(0).startsWith("Enemies")) {
      this.setDefaultInfo()
    }
  }
  
    
  /**	Creates the specified amount of spawners
    *
    * @param amount		the amount of spawners to be created
    */
  def createSpawners(amount: Int) = (0 until amount).foreach(_ => createSpawner())
  def createSpawner() = {
    var found = false
    while(!found) {
      // Generates a coordinate on the edge of the FOW
      val num = rng.nextInt()
      val x = (math.cos(num) * (radiusFOW + 4)).toInt + grid.get.base.get.coord.x
      val y = (math.sin(num) * (radiusFOW + 4)).toInt + grid.get.base.get.coord.y
      val spawner = grid.get.createSpawnPoint(Coord(x, y))
      if (spawner.isDefined) {
        found = true
      }
    }
  }
  
  /**	Enables the warning text
    *
    * @param str			the string to be displayed as a warning
    * @param frames		the number of frames the warning should remain
    */
  def setWarning(str: String, frames: Int) = {
    warningContent = str
    warningCounter = frames
    bigText.get.hidden = false
  }
  
  def pause() = this.paused = !this.paused
  
  // PRIVATE METHODS
  
  // Initializes the UI
  private def initUI() = {
    // Add UI Elements    
    val pausedTxt = new TextComponent("Game is paused", 24, Coord(0, Main.height - 48), true)
    pausedTxt.hidden = true
    pausedText = Some(pausedTxt)
    
    val boxSize = (Main.height / 8 * 0.96).toInt
    towerButtons = UIElement.generateSquareButtons(boxSize - boxSize / 4, boxSize / 16, boxSize / 16,
          Coord(0, 0), towers.length, 2, new Color(128, 128, 128), new Color(200, 200, 200), new Color(164, 164, 164))
          
    this.towers.zipWithIndex.foreach(p => {
      val (towerInfo, i) = p
      val btn = towerButtons(i)
      val radius = towerInfo.radius.get
      
      btn.iconFunc = (g: Graphics2D, x0: Int, y0: Int, size: Int) => {
        val scale = size / (radius * 2).toDouble
        for (x <- 0 until radius * 2; y <- 0 until radius * 2) {
          val diff = (Coord(x, y) - Coord(radius, radius)).length
          if (diff.toInt <= radius - 1) {
            g.setColor(if (diff.toInt == radius - 1) Color.BLACK else towerInfo.color.get)
            g.fillRect((x0 + x * scale - scale/2).toInt, (y0 + y * scale - scale/2).toInt + 1, scale.toInt + 1, scale.toInt + 1)
          }
        }
      }
      
      btn.addClickListener(_ => {
        InputHandler.clickAction = ClickAction.buyTower(towerInfo)
        InputHandler.highlightSize = towerInfo.range.get * 2
        InputHandler.coreHighlightSize = towerInfo.radius.get * 2
        InputHandler.rectHighlight = false
      })
      
      btn.addHoverListener(Some(
          _ => {
            infoTitle = towerInfo.name.get
            infoContent = Vector(
              "Price: " + towerInfo.price.get,
              "Size: " + radius,
              "Range: " + (if (towerInfo.range.get != 0) towerInfo.range.get else "INFINITY"),
              "Damage: " + towerInfo.damage.get,
              "Firerate: " + towerInfo.firerate.get
            )
          }
      ), Some(_ => setDefaultInfo()))
    })
    
    val sellBtn = new SquareButton(boxSize - boxSize / 4, boxSize / 16, boxSize / 16, Coord(Main.width - boxSize * 2 - 32, 0), new Color(128, 128, 128), new Color(200, 200, 200), new Color(164, 164, 164))
    sellBtn.icon = Some(ImageIO.read(new File("images/sell.png")))
    sellBtn.addClickListener(_ => {
      if (InputHandler.selected.isDefined) {
        InputHandler.selected.get.sell()
      }
      setDefaultInfo()
      InputHandler.selected = None
    })
      
    sellBtn.addHoverListener(Some(
        _ => {
          infoTitle = "Sell for " + (if (InputHandler.selected.isDefined) InputHandler.selected.get.price else 0)
          infoContent = Vector("This sell the tower for half the price")
        }
    ), Some(_ => setDefaultInfo()))
    sellButton = Some(sellBtn)
    
    val targetBtn = new SquareButton(boxSize - boxSize / 4, boxSize / 16, boxSize / 16, Coord(Main.width - boxSize - 32, 0), new Color(128, 128, 128), new Color(200, 200, 200), new Color(164, 164, 164))
    targetBtn.addClickListener(_ => {
      if (InputHandler.selected.isDefined) {
        InputHandler.selected.get.nextMode()
      }
      infoTitle = "Current mode: " + (if (InputHandler.selected.isDefined) InputHandler.selected.get.targetingMode + 1 else 1)
      infoContent = targetingDesc
    })
    targetBtn.addHoverListener(Some(
        _ => {
          infoTitle = "Current mode: " + (if (InputHandler.selected.isDefined) InputHandler.selected.get.targetingMode + 1 else 1)
          infoContent = targetingDesc
        }
    ), Some(_ => setDefaultInfo()))
    targetButton = Some(targetBtn)
    
    val pauseBtn = new SquareButton(boxSize - boxSize / 4, boxSize / 16, boxSize / 16, Coord(Main.width - boxSize * 2 - 32, Main.height - (boxSize * 1.5).toInt), new Color(128, 128, 128), new Color(200, 200, 200), new Color(164, 164, 164))
    pauseBtn.addClickListener(_ => this.pause())
    pauseBtn.addHoverListener(Some(
        _ => {
          infoTitle = "Time Controls"
          infoContent = Vector("Press to pause/unpause", "", "Pressing SPACE also does this")
        }
    ))
    
    pauseButton = Some(pauseBtn)
    
    val nextWaveBtn = new SquareButton(boxSize - boxSize / 4, boxSize / 16, boxSize / 16, Coord(Main.width - boxSize - 32, Main.height - (boxSize * 1.5).toInt), new Color(128, 128, 128), new Color(200, 200, 200), new Color(164, 164, 164))
    nextWaveBtn.icon = Some(ImageIO.read(new File("images/next.png")))
    nextWaveBtn.addClickListener(_ => this.startWave())
    nextWaveBtn.addHoverListener(Some(
        _ => {
          infoTitle = "Next Wave"
          infoContent = Vector("Click to start the next Wave", "", "Pressing Q also does this")
        }
    ))
    
    nextWaveButton = Some(nextWaveBtn)
    
    val infoWidth = 256
    val infoHeight = 144
    new InfoSquare(infoTitle, infoContent, Coord(0, Main.height - infoHeight - 32), Coord(infoWidth, infoHeight), 8)
    
    val offset = 128
    val fontSize = 16
    new TextComponent("Money: " + player.get.money + "€", fontSize, Coord(8, Main.height - infoHeight - 2 * fontSize - 32))
    new TextComponent("Lives: " + player.get.lives, fontSize, Coord(8, Main.height - infoHeight - 1 * fontSize - 32))
  }
  
  // Updates the UI, should be called every frame
  private def updateUI() = {
    nextWaveButton.get.interactable = this.currentWave.isEmpty
    towerButtons.zipWithIndex.foreach(pair => pair._1.interactable = this.towers(pair._2).price.get <= this.player.get.money)
    if (this.paused) {
      pausedText.get.hidden = false
      pauseButton.get.icon = Some(ImageIO.read(new File("images/play.png")))
    } else {
      pausedText.get.hidden = true
      pauseButton.get.icon = Some(ImageIO.read(new File("images/pause.png")))
    }
    if (InputHandler.selected.isDefined) {
      sellButton.get.hidden = false
      targetButton.get.hidden = false
      targetButton.get.icon = Some(targetingIcon(InputHandler.selected.get.targetingMode))
    } else {
      sellButton.get.hidden = true
      targetButton.get.hidden = true
    }
  }
  
  // Sets the infobox content to the default values
  private def setDefaultInfo() = {
    infoTitle = "Current Wave: " + wave
    infoContent = Vector("Enemies left: " + totalEnemies, "Reward: " + reward + "€")
  }
  
  /**	Inquire whether the current wave has ended or not
    *
    * @return coord		true if wave has ended
    */
  private def waveEnded = {
    grid.get.spawnPoints.forall(_.done) &&
    grid.get.enemies.length == 0
  }
}