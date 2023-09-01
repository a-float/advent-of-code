package advent

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day18 extends Day[Int] {
  override val day: Int = 18

  private case class Target(
      key: Char,
      dist: Int,
      pos: List[Int]
  )

  private def reachableTargets(start: List[Int], keys: List[Char])(implicit
      maze: String,
      width: Int
  ): List[Target] = {
    val visited = mutable.Set.empty[Int]
    val queue =
      mutable.Queue.from(start.zipWithIndex.map((pos, bot) => (pos, 0, bot)))
    val targets = ListBuffer.empty[Target]
    while (queue.nonEmpty) {
      val (pos, dist, bot) = queue.dequeue
      visited += pos
      List(-1, 1, width + 1, -width - 1)
        .map(i => (i + pos, maze(i + pos)))
        .filter((_, c) => c != '#')
        .filter((i, _) => !visited.contains(i))
        .foreach {
          case (i, c) if ('A' to 'Z').contains(c) =>
            if keys.contains(c + 'a' - 'A') then
              queue.append((i, dist + 1, bot))
          case (i, c) if ('a' to 'z').contains(c) && !keys.contains(c) =>
            targets.append(Target(c, dist + 1, start.updated(bot, i)))
          case (i, _) => queue.append((i, dist + 1, bot))
        }
    }
    targets.toList.sortBy(_.dist)
  }

  private case class CacheItem(pos: List[Int], keys: List[Char])
  private val cache = mutable.Map.empty[CacheItem, Int]

  private def explore(
      start: List[Int],
      dist: Int,
      totalKeyCount: Int,
      keys: List[Char] = List.empty,
      minDistFound: Int = Int.MaxValue
  )(implicit
      maze: String,
      width: Int
  ): Int = {
    val item = CacheItem(start, keys.sorted)
    if cache.contains(item) then {
      return dist + cache(item)
    }
    if keys.length == totalKeyCount then {
      return dist
    }
    if dist >= minDistFound then {
      return Int.MaxValue - width * width // prevent distance overflow
    }
    val targets = reachableTargets(start, keys)
    var minDist = Int.MaxValue
    targets
      .filter(t => t.key < 'a' || !keys.contains(t.key))
      .filter(t => t.key >= 'a' || keys.contains(t.key + 'a' - 'A')) foreach {
      case Target(key, d, pos) =>
        val m = explore(
          pos,
          dist + d,
          totalKeyCount,
          key :: keys,
          Math.min(minDist, minDistFound)
        )
        minDist = Math.min(m, minDist)
    }
    if minDist - dist < cache.getOrElse(item, Int.MaxValue) then
      cache.update(item, minDist - dist)
    minDist
  }

  override def part1(): Int = {
    implicit val maze: String = getSource.mkString
    implicit val width: Int = maze.indexOf('\n')
    val totalKeyCount: Int = maze.toList.count(x => x >= 'a' && x <= 'z')
    explore(maze.indexOf('@') :: Nil, 0, totalKeyCount)
  }

  override def part2(): Int = {
    var maze = getSource.mkString.toList
    implicit val width: Int = maze.indexOf('\n')
    val totalKeyCount: Int = maze.count(x => x >= 'a' && x <= 'z')
    val v = maze.indexOf('@')
    List(v - 1, v, v + 1, v - width - 1, v + width + 1).foreach(i =>
      maze = maze.updated(i, '#')
    )
    val starts = List(v - width - 2, v - width, v + width, v + width + 2)
    explore(starts, 0, totalKeyCount)(maze.mkString, width)
  }
}
