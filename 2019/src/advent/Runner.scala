package advent

object Runner {
  def main(args: Array[String]): Unit = {
    // format: off
    val days = Seq(Day1, Day2, Day3, Day4, Day5, Day6, Day7, Day8, Day9, Day10, Day11, Day12, Day13, Day14, Day15, Day16, Day17, Day18, Day19, Day20)
    // format: on
    val totalTime = days
      .sortBy(_.day)
      .map(day => {
        println(s"Day ${day.day}: ")
        val (res1, time1) = Utils.measure {
          day.part1()
        }
        println(s"  Part 1: ${time1 / 1e6}ms - $res1")

        val (res2, time2) = Utils.measure {
          day.part2()
        }
        println(s"  Part 2: ${time2 / 1e6}ms - $res2")
        time1 + time2
      })
      .sum
    println(s"\nCompleted ${days.length}/25 days in ${totalTime / 1e6}ms")
  }
}
