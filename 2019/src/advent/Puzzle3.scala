package advent
import scala.annotation.tailrec
import scala.io.Source
object Puzzle3 extends Puzzle[Int] {
  private type Wire = Map[Point, Int]
  override val day: Int = 3

  def main(args: Array[String]): Unit = {
    val src = Source.fromResource("data3.txt")
    println(part1(src))
    println(part2(src))
  }
  private def getWires(src: Source): (Wire, Wire) = {
    val list = src
      .getLines()
      .map(
        _.split(",")
          .map(d => (d.head, d.drop(1).toInt))
          .toList
      )
      .toList
    (
      cableReducer(Map.empty[Point, Int], list.head),
      cableReducer(Map.empty[Point, Int], list(1))
    )
  }

  def part1(src: Source): Int = {
    val cables = getWires(src)
    cables._1.keySet
      .intersect(cables._2.keySet)
      .map(_.manhattan)
      .filter(_ > 0)
      .min
  }

  def part2(src: Source): Int = {
    val cables = getWires(src)
    cables._1.keySet
      .intersect(cables._2.keySet)
      .map(p => cables._1.getOrElse(p, 0) + cables._2.getOrElse(p, 0))
      .filter(_ > 0)
      .min
  }

  @tailrec
  private def cableReducer(
      acc: Wire,
      elems: List[(Char, Int)],
      pos: Point = Point(0, 0)
  ): Wire = {
    if (elems.isEmpty) return acc
    val dir = elems.head._1 match {
      case 'U' => Point(0, 1)
      case 'R' => Point(1, 0)
      case 'D' => Point(0, -1)
      case 'L' => Point(-1, 0)
      case other =>
        throw new UnsupportedOperationException(s"Invalid operand $other")
    }
    val toAdd = (0 to elems.head._2)
      .map(n => (n, pos + dir * n))
      .map { case (n, p) => p -> (acc.getOrElse(pos, 0) + n) }
    cableReducer(acc ++ toAdd, elems.drop(1), pos + dir * elems.head._2)
  }
}
