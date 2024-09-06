package advent
import scala.collection.mutable.ListBuffer

object Runner {
  def fmt(l: Long) = {
    val f = l.toDouble / 1e6
    f"$f%.1fms".replace(',', '.')
  }
  def printTable(times: Seq[(Long, Long)]): Unit = {
    val buff = ListBuffer[String]()
    buff += s"| Day | Part 1 | Part 2 | Day | Part 1 | Part 2"
    buff += s"| --- | --- | --- | --- | --- | --- |"
    buff ++= LazyList
      .from(0, 1)
      .take(times.size / 2)
      .map { i =>
        val l = s"| Day ${i + 1} | ${fmt(times(i)._1)}| ${fmt(times(i)._2)} | "
        val j = i + times.size / 2
        val r = s"Day ${j + 1} | ${fmt(times(j)._1)} | ${fmt(times(j)._2)} |"
        l + r
      }
    buff += s"| Total | ${fmt(times.map(_._1).sum)} | ${fmt(times.map(_._2).sum)} |"
    println(buff.mkString("\n"))
  }

  def main(args: Array[String]): Unit = {
    // format: off
    val days = Seq(Day1, Day2, Day3, Day4, Day5, Day6, Day7, Day8, Day9, Day10, Day11, Day12, Day13, Day14, Day15, Day16, Day17, Day18, Day19, Day20, Day21, Day22, Day23, Day24)
    // format: on
    val times = days
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
        (time1, time2)
      })
    val totalTime = times.map((t1, t2) => t1 + t2).sum
    println(s"\nCompleted ${days.length}/25 days in ${totalTime / 1e6}ms")
    printTable(times)
  }
}
