package advent

object Day22 extends Day[BigInt] {
  override val day: Int = 22

  sealed trait Technique
  case class Deal() extends Technique
  case class Cut(n: BigInt) extends Technique
  case class Increment(n: BigInt) extends Technique

  val getTechniques = getSource.getLines.toList.map {
    case "deal into new stack"          => Deal()
    case comm if comm.startsWith("cut") => Cut(comm.split(" ").last.toInt)
    case comm if comm.startsWith("deal with increment") =>
      Increment(comm.split(" ").last.toInt)
  }

  override def part1(): BigInt = {
    val deckSize = 10007
    val tracked = BigInt(2019)
    getTechniques.foldLeft(tracked) {
      case (pos, Deal())       => deckSize - pos - 1
      case (pos, Cut(n))       => (pos - n + deckSize) % deckSize
      case (pos, Increment(n)) => (pos * n) % deckSize
    }
  }

  override def part2(): BigInt = {
    val deckSize = BigInt("119315717514047")
    val shuffles = BigInt("101741582076661")
    val tracked = BigInt(2020)

    def fix(x: BigInt) = (x % deckSize + deckSize) % deckSize

    def shuffle(start_seq: Tuple2[BigInt, BigInt]) =
      getTechniques.foldLeft(start_seq) {
        case (seq, Deal())       => (fix(seq._1 - seq._2), -seq._2)
        case (seq, Cut(n))       => (fix(seq._1 + n * seq._2), seq._2)
        case (seq, Increment(n)) => (seq._1, seq._2 * n.modInverse(deckSize))
      }

    val (offset, inc) = shuffle((BigInt(0), BigInt(1)))
    val finalInc = inc.modPow(shuffles, deckSize)
    val finalOffset = offset * (1 - finalInc) * (1 - inc).modInverse(deckSize)
    fix(finalOffset + (tracked * finalInc))
  }
}
