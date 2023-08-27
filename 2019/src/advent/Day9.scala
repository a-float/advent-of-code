package advent

import advent.intcode.{IntcodeComputer, Result}

import scala.io.Source

object Day9 extends Day[Result] {
  override val day: Int = 9

  def part1(): Result = getProgram(getSource).clone().execute()(1 :: Nil)

  def part2(): Result = getProgram(getSource).clone().execute()(2 :: Nil)

  private def getProgram(src: Source): IntcodeComputer =
    IntcodeComputer.loadProgram(src)
}
