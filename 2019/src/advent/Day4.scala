package advent
import scala.io.{BufferedSource, Source}

object Day4 extends Day[Int] {
  override val day: Int = 4

  def main(args: Array[String]): Unit = {
    def src: BufferedSource = Source.fromResource("data4.txt")
    println(part1(src))
    println(part2(src))
  }

  def part1(src: Source): Int =
    getPasswords(src).map(_.toString).count(isValid1)

  private def isValid1(pass: String): Boolean = {
    val (iter1, iter2) = pass.sliding(2).duplicate
    iter1.forall(p => p(0) <= p(1)) &&
    iter2.exists(p => p(0) == p(1))
  }

  def part2(src: Source): Int =
    getPasswords(src).map(_.toString).count(isValid2)

  private def getPasswords(src: Source): Range.Inclusive = {
    val ints = src.mkString.trim.split(" ").map(_.toInt)
    ints(0) to ints(1)
  }

  private def isValid2(pass: String): Boolean = {
    val (iter1, iter2) = pass.sliding(2).duplicate
    iter1.forall(p => p(0) <= p(1)) &&
    iter2.exists(p => p(0) == p(1) && pass.count(_ == p(0)) == 2)
  }
}
