package advent
import scala.io.Source

object Puzzle10 extends Puzzle[Int] {
  override val day: Int = 10

  def main(args: Array[String]): Unit = {
    val src = Source.fromResource("data10.txt")
    println(part1(src))
    println(part2(src))
  }

  def part1(src: Source): Int = getBestLocation(getSpaceMap(src))._2

  def part2(src: Source): Int = {
    val spaceMap = getSpaceMap(src)
    val spot = getBestLocation(spaceMap)._1
    val asteroid = laserSort(spot, spaceMap)(199)
    asteroid.x * 100 + asteroid.y
  }

  private def getSpaceMap(src: Source): Set[Point] = src.getLines.zipWithIndex
    .flatMap { case (line, y) =>
      line.toList.zipWithIndex.map { case (char, x) =>
        Point(x, y) -> (char == '#')
      }
    }
    .filter(_._2)
    .map(_._1)
    .toSet

  private def getBestLocation(spaceMap: Set[Point]): (Point, Int) =
    spaceMap.toList
      .map(p => (p, detectFrom(p, spaceMap)))
      .maxBy(_._2)

  private def detectFrom(point: Point, spaceMap: Set[Point]): Int =
    spaceMap
      .filter(_ != point)
      .groupBy(p => (point - p).laserAngle)
      .size

  private def laserSort(from: Point, spaceMap: Set[Point]): List[Point] =
    spaceMap.toList
      .filter(_ != from)
      .groupBy(p => (p - from).laserAngle)
      .toList
      .flatMap { case (angle, points) =>
        points.sortBy(p => (p - from).distSq).zipWithIndex.map {
          case (point, idx) =>
            (idx * 10 + angle, point)
        }
      }
      .sortBy(_._1)
      .map(_._2)
}
