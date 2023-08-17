package advent

import scala.io.{BufferedSource, Source}
import scala.language.implicitConversions

object Day8 extends Day[String] {
  override val day: Int = 8
  private val layerSize = 25 * 6
  def main(args: Array[String]): Unit = {
    def src: BufferedSource = Source.fromResource("data8.txt")
    println(part1(src))
    println(part2(src))
  }

  def part1(src: Source): String = getLayers(src)
    .map(l => (l.count(_ == '0'), l.count(_ == '1') * l.count(_ == '2')))
    .minBy(_._1)
    ._2
    .toString

  def part2(src: Source): String = "\n" + getLayers(src)
    .foldLeft(List.fill(layerSize)('2'))((acc, layer) =>
      acc.zip(layer).map(p => if (p._1 == '2') p._2 else p._1)
    )
    .map(c => if (c == '1') 'X' else ' ')
    .grouped(25)
    .map(_.mkString(""))
    .mkString("\n")

  private def getLayers(src: Source): List[String] = src.getLines.mkString.trim
    .grouped(layerSize)
    .toList
}
