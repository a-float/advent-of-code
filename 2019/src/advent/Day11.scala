package advent

import advent.intcode.{Continue, IntcodeComputer, Success}

import scala.collection.mutable

object Day11 extends Day[String] {
  override val day: Int = 11

  override def part1(): String =
    paint(IntcodeComputer.readProgramFromFile(getSource), 0).size.toString

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

  override def part2(): String = {
    val map = paint(IntcodeComputer.readProgramFromFile(getSource), 1)
    val bounds = Utils.getMapBounds(map)
    (bounds.yMin to bounds.yMax)
      .map(y =>
        (bounds.xMin to bounds.xMax)
          .map(x => if (map.getOrElse(Point(x, y), 0) == 0) " " else "#")
          .mkString
      )
      .mkString("\n", "\n", "")
  }
}
