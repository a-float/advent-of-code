package advent
import advent.intcode._

import scala.collection.mutable

object Day23 extends Day[Long] {
  override val day: Int = 23

  val program = IntcodeComputer.readProgramFromFile(getSource)
  def getInts() = Array.fill(50) { IntcodeComputer(program) }

  override def part1() = {
    val ints = getInts()
    val queue = mutable.Map[Long, List[Long]]()
    ints.zipWithIndex.map((int, i) => int.run(i :: Nil))

    while (!queue.contains(255)) {
      ints.zipWithIndex.map((int, i) => int.run(queue.getOrElse(i, -1L :: Nil)))
      queue.clear()
      ints.foreach { int =>
        int.outputs
          .grouped(3)
          .foreach { p =>
            queue(p(0)) =
              queue.getOrElse(p(0), List()) ::: p.drop(1).map(_.toLong)
          }
        int.clearOutputs()
      }
    }
    queue(255).last
  }

  override def part2() = {
    val ints = getInts()
    val queue = mutable.Map[Long, List[Long]]()
    var done = false
    var lastNAT: Option[List[Long]] = None
    var prevSentNAT: Option[List[Long]] = None
    ints.zipWithIndex.map((int, i) => int.run(i :: Nil))

    while (!done) {
      ints.zipWithIndex.map((int, i) => int.run(queue.getOrElse(i, -1 :: Nil)))

      if (queue.isEmpty && lastNAT.isDefined) {
        if (lastNAT == prevSentNAT) done = true
        prevSentNAT = lastNAT
        queue(0) = lastNAT.get
      } else {
        queue.clear()
      }

      ints.foreach { int =>
        int.outputs
          .grouped(3)
          .foreach {
            case p if p(0) == 255 => { lastNAT = Some(p.drop(1).map(_.toLong)) }
            case p =>
              queue(p(0)) =
                queue.getOrElse(p(0), List()) ::: p.drop(1).map(_.toLong)
          }
        int.clearOutputs()
      }
    }
    lastNAT.get.last
  }
}
