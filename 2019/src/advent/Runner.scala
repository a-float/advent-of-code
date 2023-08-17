package advent

import scala.io.{BufferedSource, Source}

object Runner {
  def main(args: Array[String]): Unit = {
    val puzzles = List(
      Puzzle1,
      Puzzle2,
      Puzzle3,
      Puzzle4,
      Puzzle5,
      Puzzle6,
      Puzzle7,
      Puzzle8,
      Puzzle10,
      Puzzle12
    )
    val totalTime = puzzles
      .sortBy(_.day)
      .map(puzzle => {
        def src: BufferedSource = Source.fromResource(s"data${puzzle.day}.txt")
        println(s"Day ${puzzle.day}: ")
        val (res1, time1) = Utils.time {
          puzzle.part1(src)
        }
        println(s"  Part 1: ${time1 / 1e6}ms - $res1")

        val (res2, time2) = Utils.time {
          puzzle.part2(src)
        }
        println(s"  Part 2: ${time2 / 1e6}ms - $res2")
        time1 + time2
      })
      .sum
    println(s"\nCompleted ${puzzles.length}/25 days in ${totalTime / 1e6}ms")
  }
}
