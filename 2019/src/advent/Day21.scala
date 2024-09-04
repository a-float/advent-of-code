package advent
import advent.intcode.IntcodeComputer

object Day21 extends Day[String] {
  override val day: Int = 21

  val program = IntcodeComputer.readProgramFromFile(getSource)

  def runSpringCode(code: List[String]) = {
    val int = IntcodeComputer(program)
    int.run(code.mkString("\n").toList.map(_.toLong))
    int.outputs.last match {
      case x if x <= 256 => int.outputs.map(_.toChar).mkString
      case x             => x.toString
    }
  }

  override def part1() = runSpringCode(
    // (!A || !B || !C) && D
    List(
      "OR A J",
      "AND B J",
      "AND C J",
      "NOT J J",
      "AND D J",
      "WALK\n"
    )
  )

  override def part2() = runSpringCode(
    // part1 && (E || H)
    List(
      "OR A J",
      "AND B J",
      "AND C J",
      "NOT J J",
      "AND D J",
      "OR E T",
      "OR H T",
      "AND T J",
      "RUN\n"
    )
  )
}
