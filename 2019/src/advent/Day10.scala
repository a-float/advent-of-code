package advent
import scala.io.Source

object Day10 extends Day[Int] {
  override val day: Int = 10

  def part1(): Int = getBestLocation(getSpaceMap(getSource))._2

  def part2(): Int = {
    val spaceMap = getSpaceMap(getSource)
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
