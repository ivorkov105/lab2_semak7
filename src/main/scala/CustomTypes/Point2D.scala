package CustomTypes

import scala.math.sqrt

case class Point2D private (x: Double, y: Double) extends Ordered[Point2D] {
  
  def getX: Double = x
  
  def getY: Double = y
  
  def getCoords: Array[Double] = Array(x, y)
  
  def getVector: Double = sqrt(x * x + y * y)
  
  override def compare(that: Point2D): Int = this.getVector.compareTo(that.getVector)
  
  override def toString: String = s"Point2D($x, $y)"
}

object Point2D {
  
  def of(x: Double, y: Double): Point2D = Point2D(x, y)
  
  def parse(point: String): Point2D = {
    require(point != null && point.trim.nonEmpty, "Входная строка не может быть пустой.")
    
    val parts = point.split(",")
    require(parts.length == 2, s"Строка должна быть в формате 'x,y'. Получено: $point")
    
    try {
      val x = parts(0).trim.toDouble
      val y = parts(1).trim.toDouble
      Point2D.of(x, y)
    } catch {
      case e: NumberFormatException =>
        throw new IllegalArgumentException(s"Координаты должны быть числами. Получено: $point", e)
    }
  }
}
