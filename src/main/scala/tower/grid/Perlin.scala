package tower

import scala.util.Random

// Generates a noise map with the perlin noise algorithm
class Perlin(xSize: Int, ySize: Int, octaves: Int, bias: Double) {
  private val noiseMap = Array.ofDim[Double](xSize, ySize)
  
  // Do the calculation
  def calculatePerlin(seed: Int) = {
    val random = new Random(seed)
    val maxDim = xSize.max(ySize)
    val randomTable = Array.tabulate(maxDim, maxDim)((_, _) => random.nextFloat())
    
    for (x <- 0 until xSize; y <- 0 until ySize) {
      var noise = 0.0
      var scaleAcc = 0.0
      var scale = 1.0
      
      for (o <- 0 until octaves) {
        val pitch = xSize >> o
        val sampleX1 = (x / pitch) * pitch
        val sampleY1 = (y / pitch) * pitch
        
        val sampleX2 = (sampleX1 + pitch) % xSize
        val sampleY2 = (sampleY1 + pitch) % xSize
        
        val blendX = (x - sampleX1) / pitch.toDouble
        val blendY = (y - sampleY1) / pitch.toDouble
        
        val sampleT = (1.0 - blendX) * randomTable(sampleX1)(sampleY1) + blendX * randomTable(sampleX2)(sampleY1)
        val sampleB = (1.0 - blendX) * randomTable(sampleX1)(sampleY2) + blendX * randomTable(sampleX2)(sampleY2)
        
        scaleAcc += scale
        noise += (blendY * (sampleB - sampleT) + sampleT) * scale
        scale = scale / bias
      }
      
      noiseMap(x)(y) = noise / scaleAcc
    }
    noiseMap
  }
  
  def apply(x: Int)(y: Int) = noiseMap(x)(y)
}