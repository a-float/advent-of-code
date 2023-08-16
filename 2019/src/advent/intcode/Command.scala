package advent.intcode

sealed trait Command

case class Compute(op: (Int, Int) => Int, mode1: Int, mode2: Int)
    extends Command

case class Input() extends Command

case class Output(mode1: Int) extends Command

case class Halt() extends Command

case class Crash() extends Command

case class Jump(condition: Boolean, mode1: Int, mode2: Int) extends Command

case class AdjustRelativeBase(offset: Int) extends Command
