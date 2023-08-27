package advent

import advent.intcode.{Continue, IntcodeComputer, Success}

object Day13 extends Day[String] {
  override val day: Int = 13

  override def part1(): String = {
    val intcode = IntcodeComputer.loadProgram(getSource)
    intcode.run(List.empty)
    intcode.outputs
      .grouped(3)
      .count(x => x(2) == 2)
      .toString
  }
  override def part2(): String = {
    val program = IntcodeComputer.readProgramFromFile(getSource)
    program(0) = 2
    val intcode = new IntcodeComputer(program)
    var result = intcode.run(0 :: Nil)
    var game: Map[Long, Point] = Map.empty
    while (true) {
      result match {
        case Success(_) =>
          return intcode.outputs.drop(2).grouped(3).map(_.head).max.toString
        case Continue(_) =>
          game = intcode.outputs
            .grouped(3)
            .filter(x => x(2) >= 3)
            .map(x => x(2) -> Point(x.head.toInt, x(1).toInt))
            .toMap
          val move = game(4).x.compare(game(3).x)
          intcode.clearOutputs()
          result = intcode.run(move :: Nil)
      }
    }
    "unknown"
  }

//  private def printGameState(outputs: List[Long]): Unit = {
//    val map = outputs
//      .map(_.toInt)
//      .grouped(3)
//      .map(x => Point(x.head, x(1)) -> x(2))
//      .toMap
//    val bounds = Utils.getMapBounds(map)
//    val game = (bounds.yMin to bounds.yMax)
//      .map(y =>
//        (bounds.xMin to bounds.xMax)
//          .map(x =>
//            map.getOrElse(Point(x, y), 0) match {
//              case 0     => " "
//              case 1     => "#"
//              case 2     => "B"
//              case 3     => "-"
//              case 4     => "o"
//              case score => { println(s"Score is $score"); "" }
//            }
//          )
//          .mkString
//      )
//    println(game.mkString("\n"))
//  }
}
