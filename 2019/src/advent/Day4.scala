package advent
import scala.io.Source

object Day4 extends Day[Int] {
  override val day: Int = 4

  def part1(): Int =
    getPasswords(getSource).map(_.toString).count(isValid1)

  private def isValid1(pass: String): Boolean = {
    val (iter1, iter2) = pass.sliding(2).duplicate
    iter1.forall(p => p(0) <= p(1)) &&
    iter2.exists(p => p(0) == p(1))
  }

  private def getPasswords(src: Source): Range.Inclusive = {
    val ints = src.mkString.trim.split("-").map(_.toInt)
    ints(0) to ints(1)
  }

  def part2(): Int =
    getPasswords(getSource).map(_.toString).count(isValid2)

  private def isValid2(pass: String): Boolean = {
    val (iter1, iter2) = pass.sliding(2).duplicate
    iter1.forall(p => p(0) <= p(1)) &&
    iter2.exists(p => p(0) == p(1) && pass.count(_ == p(0)) == 2)
  }
}
