package advent

import advent.intcode.IntcodeComputer

import scala.collection.mutable.ListBuffer

object Day17 extends Day[Long] {
  override val day: Int = 17

  private def getPath(maze: String): List[String] = {
    def safeMaze(i: Int) = if ((0 to maze.length).contains(i)) maze(i) else '.'
    val width = maze.indexOf('\n') + 1
    var curr = maze.indexWhere(c => List('^', '>', 'v', '<').contains(c))
    var dir = maze(curr) match {
      case '^' => 0
      case '>' => 1
      case 'v' => 2
      case '<' => 3
    }
    val res = ListBuffer.empty[String]
    while (true) {
      val dirs = List(-width, 1, width, -1).zipWithIndex
        .filter((_, i) => (i - dir).abs != 2)
        .filter((d, _) => safeMaze(curr + d) == '#')
      if (dirs.isEmpty) return res.toList
      res.append(if ((4 + dir - dirs.head._2) % 4 == 1) "L" else "R")
      dir = dirs.head._2
      val dist =
        LazyList
          .from(1)
          .takeWhile(d => safeMaze(curr + d * dirs.head._1) == '#')
          .size
      curr += dist * dirs.head._1
      res.append(dist.toString)
    }
    List.empty
  }

  override def part1(): Long = {
    val intcode = IntcodeComputer.loadProgram(getSource)
    intcode.run(List.empty)
    val maze = intcode.outputs.map(_.toChar).mkString
    val width = maze.indexOf('\n') + 1
    maze.indices
      .filter(i => maze(i) == '#')
      .filter(i =>
        List(i + 1, i - 1, i + width, i - width)
          .filter(n => n >= 0 && n < maze.length)
          .count(n => maze(n) == '#') == 4
      )
      .fold(0)((acc, i) => acc + (i % width) * (i / width))
      .toLong
  }

  private def splitPath(path: List[String]): List[String] = {
    val x = for (
      i <- 1 to 10;
      j <- 1 to 10;
      k <- 1 to 10
      if i + j + k <= path.length
    ) yield {
      val A = path.slice(0, i)
      val B = LazyList
        .iterate((path.drop(i), false)) {
          case (xs, _) if xs.startsWith(A) => (xs.drop(i), false)
          case (xs, _)                     => (xs, true)
        }
        .dropWhile(!_._2)
        .head
        ._1
        .slice(0, j)
      val C = LazyList
        .iterate((path.drop(i), false)) {
          case (xs, _) if xs.startsWith(A) => (xs.drop(i), false)
          case (xs, _) if xs.startsWith(B) => (xs.drop(j), false)
          case (xs, _)                     => (xs, true)
        }
        .dropWhile(!_._2)
        .head
        ._1
        .slice(0, k)
      val main = LazyList
        .iterate((path, List.empty[Char])) {
          case (xs, rs) if xs.startsWith(A) => (xs.drop(i), 'A' :: rs)
          case (xs, rs) if xs.startsWith(B) => (xs.drop(j), 'B' :: rs)
          case (xs, rs) if xs.startsWith(C) => (xs.drop(k), 'C' :: rs)
          case _                            => (List.empty, List.empty)
        }
        .dropWhile(_._1.nonEmpty)
        .head
        ._2
        .reverse
      List(main, A, B, C)
    }
    x.dropWhile(p => p.head.isEmpty || p.head.length > 11)
      .take(1)
      .head
      .map(_.mkString(","))
  }

  override def part2(): Long = {
    val program = IntcodeComputer.readProgramFromFile(getSource)
    program(0) = 2
    val intcode = new IntcodeComputer(program)
    intcode.run(List.empty)
    val maze = intcode.outputs.map(_.toChar).mkString
    val input = splitPath(getPath(maze)).mkString("\n") + "\nn\n"
    intcode.run(input.toList.map(_.toLong))
    intcode.outputs.last
  }
}
