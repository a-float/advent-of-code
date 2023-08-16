package advent
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Puzzle2 extends Puzzle[Try[Int]] {
  private type Program = Array[Int]
  override val day: Int = 2

  def main(args: Array[String]): Unit = {
    val src = Source.fromResource("data2.txt")
    println(part1(src))
    println(part2(src))
  }

  def part1(src: Source): Try[Int] = execute(getProgram(src).clone, 12, 2)

  def part2(src: Source): Try[Int] =
    bruteforce(getProgram(src), 0, 99, 19690720)

  private def getProgram(src: Source): Program = src.mkString.trim
    .split(',')
    .map(_.toInt)

  private def bruteforce(
      program: Program,
      from: Int,
      to: Int,
      target: Int
  ): Try[Int] = {
    for (arg1 <- from to to; arg2 <- from to to) {
      val output = execute(program.clone, arg1, arg2)
      if (output.isSuccess && output.get == target) {
        return Success(arg1 * 100 + arg2)
      }
    }
    Failure(new Exception("Program never succeeds."))
  }

  private def execute(memory: Program, arg1: Int, arg2: Int): Try[Int] = {
    memory(1) = arg1
    memory(2) = arg2
    for (chunk <- memory.grouped(4)) {
      val opResult = chunk(0) match {
        case 1  => memory(chunk(1)) + memory(chunk(2))
        case 2  => memory(chunk(1)) * memory(chunk(2))
        case 99 => -1;
        case _  => -2;
      }
      if (opResult == -1) return Success(memory(0))
      if (opResult == -2) return Failure(new UnsupportedOperationException())
      memory.update(chunk(3), opResult)
    }
    Failure(new IndexOutOfBoundsException())
  }

}
