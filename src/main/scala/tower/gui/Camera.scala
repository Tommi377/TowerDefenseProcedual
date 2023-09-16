package tower

/**	Creates a camera instance
  *
  * @param grid		reference the the grid instance this belongs to
  */
class Camera(val grid: Grid) {
  // zoom level
  var zoom = 4
  
  // Camera center location in world coordinates
  var point = Coord(grid.xSize / 2, grid.ySize / 2)
  
  // Mouse in window coordinates
  var mousePos = Coord(0, 0)
  
  // Mouse in world coordinates
  def mouseCoord: Coord = mousePos.toWorld
}