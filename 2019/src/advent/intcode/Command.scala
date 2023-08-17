package advent.intcode

sealed trait Command

case class Compute(
    op: (Long, Long) => Long,
    mode1: Long,
    mode2: Long,
    mode3: Long
) extends Command

case class Input(mode1: Long) extends Command

case class Output(mode1: Long) extends Command

case class Halt() extends Command

case class Crash() extends Command

case class Jump(condition: Boolean, mode1: Long, mode2: Long) extends Command

case class AdjustRelativeBase(offset: Long) extends Command
