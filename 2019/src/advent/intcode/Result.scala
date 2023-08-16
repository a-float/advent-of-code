package advent.intcode

sealed trait Result

case class Success(value: Int) extends Result

case class Failure(message: String) extends Result

case class Continue(computer: IntcodeComputer) extends Result
