package advent

import scala.io.Source

trait Puzzle[O] {
  val day: Int
  def part1(input: Source): O
  def part2(input: Source): O
}
