package advent
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Day2 extends Day[Try[Int]] {
  private type Program = Array[Int]
  override val day: Int = 2

  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  def part1(): Try[Int] = execute(getProgram(getSource).clone, 12, 2)

  def part2(): Try[Int] =
    bruteforce(getProgram(getSource), 0, 99, 19690720)

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
