package advent

import scala.io.Source
import scala.language.implicitConversions

object Day8 extends Day[String] {
  override val day: Int = 8
  private val layerSize = 25 * 6
  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  def part1(): String = getLayers(getSource)
    .map(l => (l.count(_ == '0'), l.count(_ == '1') * l.count(_ == '2')))
    .minBy(_._1)
    ._2
    .toString

  private def getLayers(src: Source): List[String] = src.getLines.mkString.trim
    .grouped(layerSize)
    .toList

  def part2(): String = "\n" + getLayers(getSource)
    .foldLeft(List.fill(layerSize)('2'))((acc, layer) =>
      acc.zip(layer).map(p => if (p._1 == '2') p._2 else p._1)
    )
    .map(c => if (c == '1') 'X' else ' ')
    .grouped(25)
    .map(_.mkString(""))
    .mkString("\n")
}
