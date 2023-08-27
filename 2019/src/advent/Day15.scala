package advent

import advent.intcode.IntcodeComputer

import scala.collection.mutable
import scala.io.Source

object Day15 extends Day[String] {
  override val day: Int = 15

  private enum State:
    case Free, Wall, Target

  private case class Tile(pos: Point, dist: Int) extends Ordered[Tile] {
    def compare(that: Tile): Int = this.dist.compareTo(that.dist)
  }
  private def reverseMove(dir: Int) = dir match {
    case 1 => 2
    case 2 => 1
    case 3 => 4
    case 4 => 3
  }

  def move(pos: Point, dir: Int): Point = dir match
    case 1 => Point(pos.x, pos.y - 1)
    case 2 => Point(pos.x, pos.y + 1)
    case 3 => Point(pos.x + 1, pos.y)
    case 4 => Point(pos.x - 1, pos.y)

  private lazy val exploredMaze: Map[Point, State] = {
    implicit val intcode: IntcodeComputer =
      IntcodeComputer.loadProgram(getSource)
    val map = mutable.Map.empty[Point, State]
    explore(Point(0, 0), map)
    map.toMap
  }
  override def part1(): String =
    getDistToALl(Point(0, 0), exploredMaze)(
      exploredMaze.find(pair => pair._2 == State.Target).get._1
    ).toString

  override def part2(): String =
    getDistToALl(
      exploredMaze.find(pair => pair._2 == State.Target).get._1,
      exploredMaze
    ).values.max.toString

  private def explore(pos: Point, map: mutable.Map[Point, State])(implicit
      intcode: IntcodeComputer
  ): Unit = {
    intcode.clearOutputs() // 322ms -> 79ms
    (1 to 4)
      .filter(dir => !map.contains(move(pos, dir)))
      .foreach(dir => {
        intcode.run(dir :: Nil)
        intcode.outputs.last.toInt match {
          case 0 => map.update(move(pos, dir), State.Wall)
          case 1 =>
            map.update(move(pos, dir), State.Free)
            explore(move(pos, dir), map)
            intcode.run(reverseMove(dir) :: Nil)
          case 2 =>
            map.update(move(pos, dir), State.Target)
            explore(move(pos, dir), map)
            intcode.run(reverseMove(dir) :: Nil)
        }
      })
  }
  private def getDistToALl(
      start: Point,
      map: Map[Point, State]
  ): Map[Point, Int] = {
    val queue = mutable.PriorityQueue(Tile(start, 0))
    val visited = mutable.Set.empty[Point]
    val dists = mutable.Map.empty[Point, Int]
    while (queue.nonEmpty) {
      val tile = queue.dequeue()
      if !visited.contains(tile.pos) then {
        visited += tile.pos
        dists(tile.pos) = tile.dist
        (1 to 4)
          .map(dir => move(tile.pos, dir))
          .filter(map(_) != State.Wall)
          .foreach(pos => queue += Tile(pos, tile.dist + 1))
      }
    }
    dists.toMap
  }
}
