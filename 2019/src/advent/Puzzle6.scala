package advent
import scala.annotation.tailrec
import scala.io.Source

object Puzzle6 extends Puzzle[Int] {
  private type Vertex = String
  private type Graph = Map[Vertex, Vertex]
  override val day: Int = 6

  def main(args: Array[String]): Unit = {
    val src = Source.fromResource("data6.txt")
    println(part1(src))
    println(part2(src))
  }
  private def getGraph(src: Source): Graph = src.getLines
    .map(_.split(')'))
    .map(p => p(1) -> p(0))
    .toMap

  def part1(src: Source): Int = {
    val graph = getGraph(src)
    graph.keys.toList.map(getOrbitPath(graph, _).size).sum
  }

  def part2(src: Source): Int = {
    val graph = getGraph(src)
    val santaPath = getOrbitPath(graph, "SAN").zipWithIndex.toMap
    val (planet, youLength) = getOrbitPath(graph, "YOU").zipWithIndex.find {
      case (s, _) => santaPath.contains(s)
    }.get
    youLength + santaPath(planet) - 2
  }

  @tailrec
  private def getOrbitPath(
      graph: Graph,
      root: String,
      acc: List[String] = List.empty
  ): List[String] =
    root match {
      case "COM" => acc
      case other => getOrbitPath(graph, graph(other), acc :+ other)
    }
}
