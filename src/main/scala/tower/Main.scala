package tower
import javax.swing.Timer
import java.util.TimerTask
import scala.util.Random

object Main extends App{
  System.setProperty("sun.java2d.opengl", "true")
  
  // Read the config file
  val settings: GameSettings = Parser.readConfig("config/Config.txt")
  
  def width = if (settings != null) settings.windowWidth else 1280
  def height = if (settings != null) settings.windowHeight else 720

  val frameRate = 30
  
  var currentFrame = 0
  
  val game = new Game(settings)
  game.initGame()
  
  val win = new Window(width, height, game.camera.get)
  win.main(args)
  win.top.title = "Tower Defense"
  
  // The main loop
  def gameLoop() = new javax.swing.AbstractAction() {
    def actionPerformed(e: java.awt.event.ActionEvent) = {
      currentFrame += 1
      game.update()
      win.graphics.repaint()
    }
  }

  val timer = new Timer(1000 / frameRate, this.gameLoop())
  timer.setRepeats(true)
  this.timer.start()
}