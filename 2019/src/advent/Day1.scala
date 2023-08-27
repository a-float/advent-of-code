package advent
import scala.annotation.tailrec
import scala.io.Source

object Day1 extends Day[Int] {
  override val day: Int = 1

  def part1(): Int = getNumbers(getSource)
    .map(_ / 3 - 2)
    .sum

  private def getNumbers(src: Source): List[Int] = src.map(_.toInt).toList

  def part2(): Int = {
    @tailrec
    def calcFuel(fuel: Int, acc: Int = 0): Int =
      if (fuel > 0) calcFuel(fuel / 3 - 2, acc + fuel) else acc

    getNumbers(getSource)
      .map(f => calcFuel(f / 3 - 2))
      .sum
  }
}
