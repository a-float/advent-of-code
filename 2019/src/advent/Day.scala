package advent

import scala.io.Source

trait Day[O] {
  val day: Int
  def part1(src: Source): O
  def part2(src: Source): O
}
