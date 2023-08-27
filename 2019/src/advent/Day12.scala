package advent

import scala.annotation.tailrec
import scala.io.Source
import scala.language.implicitConversions
import scala.util.matching.Regex

case class Planet(pos: Vector3, vel: Vector3) {
  def getEnergy: Int = pos.toList.map(_.abs).sum * vel.toList.map(_.abs).sum

  def getAxisHash(idx: Int): String =
    pos.toList(idx) + ":" + vel.toList(idx)
}
object Day12 extends Day[BigInt] {

  private type SystemState = List[Planet]
  override val day: Int = 12
  private val positionRegex: Regex = "(\\w+)=(-?\\d+)".r

  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }
  private implicit def toVec3(list: List[Int]): Vector3 =
    Vector3(list.head, list(1), list(2))

  def part1(): BigInt =
    manySteps(getStartingState(getSource), 1000).map(_.getEnergy).sum

  def part2(): BigInt =
    lcm((0 to 2).map(getAxisCycle(getStartingState(getSource), _)))

  private def getStartingState(src: Source): List[Planet] = {
    val input = src.getLines().mkString
    val matches = positionRegex.findAllMatchIn(input).toList
    val positions = matches
      .grouped(3)
      .map(m => m.take(3).map(_.group(2).toInt): Vector3)
      .toList
    positions.map(Planet(_, Vector3.zero))
  }

  private def getAxisCycle(start: SystemState, axis: Int): Int = LazyList
    .iterate((start, 0)) { case (s, i) => (step(s), i + 1) }
    .dropWhile(x => !compareStatesOnAxis(start, x._1, axis) || x._2 == 0)
    .head
    ._2

  private def compareStatesOnAxis(
      s1: SystemState,
      s2: SystemState,
      i: Int
  ): Boolean =
    s1.map(_.getAxisHash(i)) == s2.map(_.getAxisHash(i))

  private def step(state: SystemState): SystemState = {
    state.map(planet => {
      val vel = planet.vel + state
        .map(_.pos)
        .fold(Vector3.zero)((acc, pos) => acc + (pos - planet.pos).sign)
      Planet(planet.pos + vel, vel)
    })
  }

  private def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(1: BigInt) {
    (a, b) =>
      b * a / LazyList
        .iterate((a, b)) { case (x, y) => (y, x % y) }
        .dropWhile(_._2 != 0)
        .head
        ._1
        .abs
  }

  @tailrec
  private def manySteps(state: SystemState, n: Int): SystemState =
    if (n == 1) step(state) else manySteps(step(state), n - 1)
}

//val iterativePart2 = time {
//  var cycles = 0 :: 0 :: 0 :: Nil
//  var stepCount = 1
//  var state = step(startingState)
//  while (cycles.contains(0)) {
//    for (i <- 0 to 2) {
//      if (cycles(i) == 0 && compareStatesOnAxis(startingState, state, i))
//        cycles = cycles.updated(i, stepCount)
//    }
//    state = step(state)
//    stepCount += 1
//  }
//  lcm(cycles.map(BigInt(_)))
//}
