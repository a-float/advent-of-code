package advent
import scala.annotation.tailrec
import scala.io.Source

object Day1 extends Day[Int] {
  override val day: Int = 1

  def main(args: Array[String]): Unit = {
    def src = Source.fromResource("data1.txt")
    println(part1(src))
    println(part2(src))
  }

  def part1(src: Source): Int = getNumbers(src)
    .map(_ / 3 - 2)
    .sum

  private def getNumbers(src: Source): List[Int] = src.map(_.toInt).toList

  def part2(src: Source): Int = {
    @tailrec
    def calcFuel(fuel: Int, acc: Int = 0): Int =
      if (fuel > 0) calcFuel(fuel / 3 - 2, acc + fuel) else acc

    getNumbers(src)
      .map(f => calcFuel(f / 3 - 2))
      .sum
  }
}
