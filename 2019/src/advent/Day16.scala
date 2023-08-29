package advent

import advent.intcode.IntcodeComputer

import scala.collection.mutable
import scala.io.Source

object Day16 extends Day[String] {
  override val day: Int = 16

  private def mult(sums: Array[Int], order: Int, offset: Int): Int = {
    var total = 0
    var i = order - 1 - offset // first 1
    while (i < sums.length) {
      total += sums(Math.min(sums.length - 1, i + order)) - sums(i)
      i += 4 * order
    }
    i = order * 3 - 1 - offset // first -1
    while (i < sums.length) {
      total -= sums(Math.min(sums.length - 1, i + order)) - sums(i)
      i += 4 * order
    }
    total
  }
  
  private def fft(input: Array[Int], offset: Int = 0): Array[Int] = {
    val sums =
      input.foldLeft(List(0))((a, x) => (a.head + x) :: a).reverse.toArray
    (1 to input.length)
      .map(i => mult(sums, i + offset, offset).abs % 10)
      .toArray
  }

  override def part1(): String = {
    val input = getSource.mkString.trim.split("").toList.map(_.toInt).toArray
    (1 to 100).foldLeft(input)((acc, _) => fft(acc)).take(8).mkString("")
  }

  override def part2(): String = {
    val input = getSource.mkString.trim.split("").toList.map(_.toInt).toArray
    val toSkip = input.take(7).mkString("").toInt
    val longInput = Array.fill(10000)(input).flatten.drop(toSkip)
    (1 to 100)
      .foldLeft(longInput)((acc, _) => fft(acc, offset = toSkip))
      .take(8)
      .mkString("")
  }
}
