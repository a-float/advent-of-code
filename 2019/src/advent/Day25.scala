package advent
import advent.intcode._
import scala.io.StdIn.readLine
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day25 extends Day[String] {
  override val day: Int = 25
  val historyPath = "history.txt"

  val items = Set(
    "candy cane",
    "coin",
    "fuel cell",
    "hypercube",
    "mutex",
    "spool of cat6",
    "tambourine",
    "weather machine"
  )

  def stol(str: String) = str.toList.map(_.toLong)
  def ltos(lst: List[Long]) = lst.map(_.toChar).mkString

  override def part1() = {
    val int = IntcodeComputer.loadProgram(getSource)
    int.run(List.empty)
    val history = ListBuffer[String]()
    var command = ""
    while (command != "quit" && int.outputs.nonEmpty) {
      println(ltos(int.outputs))
      int.clearOutputs()
      command = readLine().trim()
      history += command
      command match {
        case "save" =>
          new PrintWriter(historyPath) {
            write(history.mkString("\n")); close
          }
        case "load" =>
          Source.fromResource(historyPath).getLines.map { _.trim }.foreach {
            msg => int.run(stol(msg + "\n"))
          }
        case "brute" => {
          val msgs = ListBuffer[String]()
          val correct = items.subsets
            .takeWhile { sub =>
              items.foreach { item => msgs += s"drop $item\n" }
              sub.foreach { item => msgs += s"take $item\n" }
              msgs.foreach { msg => int.run(stol(msg)) }
              int.clearOutputs()
              int.run(stol("west\n"))
              println(s"Trying $sub")
              ltos(int.outputs).contains("== Security Checkpoint ==")
            }
            .toList
            .last
          println(s"Solution found!")
        }
        case command => int.run(stol(command + "\n"))
      }
    }
    "Done!"
  }

  override def part2() = "Done!"

}
