package advent
import scala.annotation.tailrec
import scala.io.Source

object Day6 extends Day[Int] {
  private type Vertex = String
  private type Graph = Map[Vertex, Vertex]
  override val day: Int = 6

  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  def part1(): Int = {
    val graph = getGraph(getSource)
    graph.keys.toList.map(getOrbitPath(graph, _).size).sum
  }

  def part2(): Int = {
    val graph = getGraph(getSource)
    val santaPath = getOrbitPath(graph, "SAN").zipWithIndex.toMap
    val (planet, youLength) = getOrbitPath(graph, "YOU").zipWithIndex.find {
      case (s, _) => santaPath.contains(s)
    }.get
    youLength + santaPath(planet) - 2
  }

  private def getGraph(src: Source): Graph = src.getLines
    .map(_.split(')'))
    .map(p => p(1) -> p(0))
    .toMap

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
