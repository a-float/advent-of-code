package advent

import scala.collection.mutable

object Day20 extends Day[Int] {
  override val day: Int = 20

  private case class Tile(pos: Int, recursion: Int)
  private case class PlutoMaze(
      map: Map[Int, List[Tile]],
      portals: Map[String, (Int, Int)]
  )

  private lazy val pluto: PlutoMaze = {
    val str = getSource.mkString
    val width = str.indexOf('\n') + 1
    val height = str.count(_ == '\n')
    val portals = mutable.Map.empty[String, (Int, Int)]
    val map = mutable.Map.empty[Int, List[Tile]].withDefaultValue(List.empty)
    str.zipWithIndex
      .filter(_._1 >= 'A')
      .foreach((_, i) => {
        List(1, -1, width, -width)
          .filter(d =>
            str.indices.contains(i + d) && str.indices.contains(i - d)
          )
          .filter(d => str(i + d) >= 'A')
          .filter(d => str(i - d) == '.')
          .foreach(d => {
            val id = List(i + d, i).sorted.map(x => str(x)).mkString
            if portals.contains(id) then {
              val (other, recursion) = portals(id)
              map.update(i - d, Tile(other, -recursion) :: Nil)
              map.update(other, Tile(i - d, recursion) :: Nil)
            } else {
              val x = i % width
              val y = i / width
              val isOuter = x < 5 || x > width - 5 || y < 5 || y > height - 5
              portals.update(id, (i - d, if isOuter then -1 else 1))
            }
          })
      })

    str.zipWithIndex.filter(_._1 == '.').foreach { (c, i) =>
      val valid = List(1, -1, width, -width)
        .filter(d => str(i + d) == '.')
        .map(d => Tile(i + d, 0))
      map.update(i, map(i) ::: valid)
    }

    PlutoMaze(map.toMap, portals.toMap)
  }

  private def bfs(
      startPos: Int,
      endPos: Int,
      map: Map[Int, List[Tile]],
      withRecursion: Boolean = false,
      maxDepth: Int = Int.MaxValue
  ): List[Tile] = {
    val (start, end) = (Tile(startPos, 0), Tile(endPos, 0))
    val visited = mutable.Set.empty[Tile]
    val queue = mutable.Queue.from((start, List.empty[Tile]) :: Nil)
    while (queue.nonEmpty) {
      val (tile, path) = queue.dequeue
      if (tile == end) return tile :: path
      visited += tile
      map(tile.pos)
        .map(n =>
          Tile(n.pos, if (withRecursion) n.recursion + tile.recursion else 0)
        )
        .filter(n => !visited.contains(n))
        .filter(n => (0 to maxDepth).contains(n.recursion))
        .foreach(n => queue.append((n, tile :: path)))
    }
    List.empty
  }

  override def part1(): Int =
    bfs(pluto.portals("AA")._1, pluto.portals("ZZ")._1, pluto.map).size - 1

  override def part2(): Int =
    bfs(
      pluto.portals("AA")._1,
      pluto.portals("ZZ")._1,
      pluto.map,
      true,
      pluto.portals.size
    ).size - 1
}
