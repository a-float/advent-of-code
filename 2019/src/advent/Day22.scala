package advent

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day22 extends Day[Int] {
  override val day: Int = 22

//  @tailrec
//  private def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)
//
//  private def lcm(a: Int, b: Int): Int = a / gcd(a, b) * b

  override def part1(): Int = Utils.time {
    val deckSize = 10007
    val tracked = 2019
    getSource.getLines.toList.foldLeft(tracked)((pos, shuffle) =>
      shuffle match {
        case "deal into new stack" => deckSize - pos - 1

        case comm if comm.startsWith("cut") =>
          val cut = comm.split(" ").last.toInt
          val absCut = (deckSize + cut) % deckSize
          if absCut > pos then pos + deckSize - absCut
          else pos - absCut

        case comm if comm.startsWith("deal with increment") =>
          val inc = comm.split(" ").last.toInt
          (pos * inc) % deckSize
      }
    )
  }

  override def part2(): Int = Utils.time {
    val deckSize = BigInt("119315717514047")
    val trackedPos = BigInt(2020)
    val shuffles = BigInt("101741582076661")

    def findPrev(trackedPos: BigInt): List[BigInt] =
      getSource.getLines.toList.reverse.foldLeft(trackedPos :: Nil)(
        (acc, shuffle) =>
          val pos = acc.head
          shuffle match {
            case "deal into new stack" => deckSize - pos - 1 :: acc

            case comm if comm.startsWith("cut") =>
              val cut = -comm.split(" ").last.toInt
              val absCut = (deckSize + cut) % deckSize
              if absCut > pos then pos + deckSize - absCut :: acc
              else pos - absCut :: acc

            case comm if comm.startsWith("deal with increment") =>
              val inc = comm.split(" ").last.toInt
              //            val (incMod, decMod) = (pos % inc, deckSize % inc)
              val mult = LazyList
                .from(0)
                .dropWhile(m => (m * deckSize + pos) % inc != 0)
                .head
              //            println(s"$incMod, $decMod, mult is $mult")
              (mult * deckSize + pos) / inc :: acc
          }
      )
//    println(s"Result is ${findPrev(2020)}")
    val l =
      LazyList
        .iterate(trackedPos :: Nil)(prev => findPrev(prev.head).reverse)
        .drop(1)
        .flatten
        .take(1000)
        .toList
    println(l.groupBy(identity).collect { case (x, List(_, _, _*)) => x })
    println(l)
    println(l.size)
    2
  }
}

//    val x = getSource.getLines.foldLeft(List.from(0 until deckSize))(
//      (deck, shuffle) =>
//        //        println(deck)
//        shuffle match {
//          case "deal into new stack" => deck.reverse
//
//          case comm if comm.startsWith("cut") =>
//            val cut = comm.split(" ").last.toInt
//            val absCut = if cut > 0 then cut else deck.size + cut
//            deck.drop(absCut) ::: deck.take(absCut)
//
//          case comm if comm.startsWith("deal with increment") =>
//            val inc = comm.split(" ").last.toInt
//            val arr = Array.fill(deck.size)(-8)
//            deck.indices.foreach(i => {
//              arr((i * inc) % deck.size) = deck(i)
//            })
//            arr.toList
//        }
//    )
//
//    println(x)
