package advent
import advent.intcode.{IntcodeComputer, Result}

import scala.io.Source

object Puzzle5 extends Puzzle[Result] {
  override val day: Int = 5

  def main(args: Array[String]): Unit = {
    val src = Source.fromResource("data5.txt")
    println(part1(src))
    println(part2(src))
  }

  def part1(src: Source): Result = getProgram(src).clone().execute()(1 :: Nil)

  def part2(src: Source): Result = getProgram(src).clone().execute()(5 :: Nil)

  private def getProgram(src: Source): IntcodeComputer =
    IntcodeComputer.loadProgram(src)
}
