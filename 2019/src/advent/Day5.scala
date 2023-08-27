package advent
import advent.intcode.{IntcodeComputer, Result}

import scala.io.Source

object Day5 extends Day[Result] {
  override val day: Int = 5

  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  def part1(): Result = getProgram(getSource).clone().execute()(1 :: Nil)

  def part2(): Result = getProgram(getSource).clone().execute()(5 :: Nil)

  private def getProgram(src: Source): IntcodeComputer =
    IntcodeComputer.loadProgram(src)
}
