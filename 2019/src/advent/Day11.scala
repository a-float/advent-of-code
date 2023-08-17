package advent

import scala.io.{BufferedSource, Source}

object Day11 extends Day[Int] {
  override val day: Int = 11

  def main(args: Array[String]): Unit = {
    def src: BufferedSource = Source.fromResource("data10.txt")
    println(part1(src))
    println(part2(src))
  }

  override def part1(input: Source): Int = 1

  override def part2(input: Source): Int = 2
}
