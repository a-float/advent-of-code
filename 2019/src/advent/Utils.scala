package advent

import scala.language.implicitConversions

object Vector3 {
  def zero: Vector3 = Vector3(0, 0, 0)
}
case class Vector3(x: Int, y: Int, z: Int) {
  def -(that: Vector3): Vector3 =
    Vector3(this.x - that.x, this.y - that.y, this.z - that.z)
  def +(that: Vector3): Vector3 =
    Vector3(this.x + that.x, this.y + that.y, this.z + that.z)
  def sign: Vector3 = Vector3(this.x.sign, this.y.sign, this.z.sign)
  def toList: Seq[Int] = List(this.x, this.y, this.z)

  override def toString: String = s"[$x, $y, $z]"
}

case class Point(x: Int, y: Int) {
  def +(that: Point): Point = Point(this.x + that.x, this.y + that.y)

  def -(that: Point): Point = Point(this.x - that.x, this.y - that.y)

  def *(that: Int): Point = Point(this.x * that, this.y * that)

  def /(that: Int): Point = Point(this.x / that, this.y / that)

  def laserAngle: Double = -Math.atan2(this.x, this.y) + Math.PI

  def distSq: Int = this.x * this.x + this.y * this.y

  def manhattan: Int = this.x.abs + this.y.abs

  override def equals(obj: Any): Boolean = obj match {
    case Point(x, y) => x.equals(this.x) && y.equals(this.y)
    case _           => false
  }
}
object Utils {
  def getMapBounds(map: Map[Point, Int]): Bounds = map.keys
    .foldLeft(Bounds(0, 0, 0, 0))((acc, point) =>
      Bounds(
        Math.min(acc.xMin, point.x),
        Math.max(acc.xMax, point.x),
        Math.min(acc.yMin, point.y),
        Math.max(acc.yMax, point.y)
      )
    )

  def time[R](block: => R): (R, Long) = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
//    println("Elapsed time: " + (t1 - t0) / 1e6 + "ms")
    (result, t1 - t0)
  }

  case class Bounds(xMin: Int, xMax: Int, yMin: Int, yMax: Int)
}
