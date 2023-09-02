package advent
import advent.intcode.{IntcodeComputer, Success}

object Day19 extends Day[Int] {
  override val day: Int = 19

  private case class Slice(col: Int, row: Int, width: Int)

  private def slices(implicit intcode: IntcodeComputer) = {
    LazyList.iterate(Slice(0, 0, 1))(prevRow => {
      val start = Math.max(prevRow.col - 1, -1)
      val x = LazyList
        .iterate((0, start)) { prev =>
          intcode.clone().run(List(prev._2 + 1, prevRow.row + 1)) match {
            case Success(1) => (1, prev._2 + 1)
            case _          => (0, prev._2 + 1)
          }
        }
        .take(prevRow.width + 10)
        .toList

      Slice(
        Math.max(0, prevRow.col + x.indexWhere(_._1 == 1) - 1),
        prevRow.row + 1,
        x.count(_._1 == 1)
      )
    })
  }

  override def part1(): Int = {
    implicit val int: IntcodeComputer = IntcodeComputer.loadProgram(getSource)
    slices.take(50).map(_.width).sum
  }

  override def part2(): Int = {
    implicit val int: IntcodeComputer = IntcodeComputer.loadProgram(getSource)
    slices
      .dropWhile(_.width < 100)
      .sliding(100)
      .flatMap(window =>
        List.tabulate(window.head.width - 100 + 1)(i =>
          (window.head.col + i, window)
        )
      )
      .dropWhile((x, window) => {
        !window.forall(s => s.col <= x && s.col + s.width >= x + 100)
      })
      .map((x, window) => x * 10000 + window.head.row)
      .next()
  }
}
