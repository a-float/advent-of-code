package advent

import scala.io.{BufferedSource, Source}

trait Day[O] {
  val day: Int
  def getSource: BufferedSource = Source.fromResource(s"data$day.txt")
  def part1(): O
  def part2(): O
  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

}
