package advent

import advent.intcode.{IntcodeComputer, Result}

import scala.io.{BufferedSource, Source}

object Puzzle9 extends Puzzle[Result] {
  override val day: Int = 9

  def main(args: Array[String]): Unit = {
    def src: BufferedSource = Source.fromResource("data9.txt")
    println(part1(src))
    println(part2(src))
  }

  def part1(src: Source): Result = getProgram(src).clone().execute()(1 :: Nil)

  def part2(src: Source): Result = getProgram(src).clone().execute()(2 :: Nil)

  private def getProgram(src: Source): IntcodeComputer =
    IntcodeComputer.loadProgram(src)
}
