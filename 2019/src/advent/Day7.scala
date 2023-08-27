package advent

import advent.intcode._

import scala.io.Source

object Day7 extends Day[Long] {
  override val day: Int = 7
  private val startInput = 0

  def part2(): Long = {
    val program = getProgram(getSource)
    (5 to 9).permutations
      .map(findMaxSignal(program, _))
      .collect { case Success(value) => value }
      .max
  }

  private def findMaxSignal(
      process: IntcodeComputer,
      perm: Iterable[Int]
  ): Result = {
    val ps = perm
      .map { phase =>
        process.clone().run(phase :: Nil)
      }
      .collect { case Continue(process) => process }
      .toArray
    assert(perm.size == ps.length)
    val MAX_ITER = 1000
    var i = 0
    while (i < MAX_ITER) {
      val currIdx = i % ps.length
      val prevProc = ps((ps.length + i - 1) % ps.length)
      val inputs = if (i == 0) 0L :: prevProc.outputs else prevProc.outputs
      ps(currIdx).clone().run(inputs) match {
        case Continue(process) =>
          ps(currIdx) = process
          i += 1
        case Success(value) if i % ps.length < ps.length - 1 =>
          ps(currIdx) = ps(currIdx).clone()
          ps(currIdx).outputs = value :: Nil
          i += 1
        case other => return other
      }
    }
    throw new Error(s"Unable to determine signal in $MAX_ITER iterations")
  }

  private def getProgram(src: Source): IntcodeComputer =
    IntcodeComputer.loadProgram(src)

  def part1(): Long = {
    val process = getProgram(getSource)
    (0 to 4).permutations
      .map(perm =>
        perm.foldLeft((x: Result) => x)((acc: Result => Result, phase: Int) =>
          acc andThen {
            case Success(value) =>
              process.clone().execute()(phase :: value :: Nil)
            case Failure(message)  => Failure(message)
            case Continue(process) => Continue(process)
          }
        )
      )
      .map(func => func(Success(startInput)))
      .collect { case Success(signal) => signal }
      .max
  }

}
