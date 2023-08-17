package advent

import advent.intcode.{Continue, IntcodeComputer, Success}

import scala.collection.mutable
import scala.io.{BufferedSource, Source}

object Day11 extends Day[String] {
  override val day: Int = 11

  def main(args: Array[String]): Unit = {
    def src: BufferedSource = Source.fromResource("data11.txt")
    println(part1(src))
    println(part2(src))
  }

  override def part1(src: Source): String =
    paint(IntcodeComputer.readProgramFromFile(src), 0).size.toString

  override def part2(src: Source): String = {
    val map = paint(IntcodeComputer.readProgramFromFile(src), 1)
    val (xMin, xMax, yMin, yMax) =
      map.keys
        .foldLeft((0, 0, 0, 0))((acc, point) =>
          Tuple4(
            Math.min(acc._1, point.x),
            Math.max(acc._2, point.x),
            Math.min(acc._3, point.y),
            Math.max(acc._4, point.y)
          )
        )
    (yMin to yMax)
      .map(y =>
        (xMin to xMax)
          .map(x => if (map.getOrElse(Point(x, y), 0) == 0) " " else "#")
          .mkString
      )
      .mkString("\n", "\n", "")
  }

  private def paint(
      program: Array[Long],
      startTileColor: Int
  ): Map[Point, Int] = {
    val map = mutable.Map(Point(0, 0) -> startTileColor)
    var pos = Point(0, 0)
    var dir = 0
    var result = new IntcodeComputer(program)
      .run(startTileColor :: Nil)
    while (true) {
      result = result match {
        case Continue(process) => {
          val outs = process.outputs
          map(pos) = outs.head.toInt
          dir = (dir + 4 + outs(1).toInt * 2 - 1) % 4
          pos = dir match {
            case 0 => Point(pos.x, pos.y - 1)
            case 1 => Point(pos.x + 1, pos.y)
            case 2 => Point(pos.x, pos.y + 1)
            case 3 => Point(pos.x - 1, pos.y)
          }
          process
            .clone()
            .run(map.getOrElse(pos, 0) :: Nil)
        }
        case Success(_) => return map.toMap
      }
    }
    map.toMap
  }
}
