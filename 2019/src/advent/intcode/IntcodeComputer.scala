package advent.intcode

import advent.intcode.IntcodeComputer.Program

import scala.collection.mutable.ListBuffer
import scala.io.Source

object IntcodeComputer {
  private type Program = Array[Int]

  def loadProgramFromFile(filename: String): IntcodeComputer =
    new IntcodeComputer(
      readProgramFromFile(
        Source
          .fromResource(filename)
      )
    )

  def loadProgram(src: Source): IntcodeComputer =
    new IntcodeComputer(readProgramFromFile(src))

  private def readProgramFromFile(src: Source): Program = src.mkString
    .filterNot(_.isWhitespace)
    .trim
    .split(',')
    .map(_.toInt)
}
class IntcodeComputer(val program: Array[Int]) extends Cloneable {
  var memory: Program = program.clone()
  var pc = 0 // program counter
  private var _outputs: ListBuffer[Int] = ListBuffer.empty[Int]
  private var rb = 0 // relative base

  override def clone(): IntcodeComputer = {
    val computer = new IntcodeComputer(memory)
    computer.pc = pc
    computer.rb = rb
    computer
  }

  def execute(waitForInputs: Boolean = false)(
      inputs: List[Int]
  ): Result = {
    var inputIdx = 0
    while (0 <= pc && pc < memory.length) {
//      println(
//        s"${parseCommand(memory(pc))} $pc $rb ${memory.mkString("Array(", ", ", ")")}"
//      )
      parseCommand(memory(pc)) match {
        case Compute(op, mode1, mode2) => {
          val arg1 = getArgument(pc + 1, mode1)
          val arg2 = getArgument(pc + 2, mode2)
          memory(memory(pc + 3)) = op(arg1, arg2)
          pc += 4
        }
        case Input() =>
          if (inputIdx >= inputs.size) {
            if (waitForInputs) {
              return Continue(this)
            } else {
              return Failure("Out of inputs")
            }
          }
          memory(memory(pc + 1)) = inputs(inputIdx)
          inputIdx += 1
          pc += 2
        case Output(mode1) =>
          val arg = getArgument(pc + 1, mode1)
          _outputs += arg
          pc += 2
        case Jump(condition, mode1, mode2) => {
          val arg1 = getArgument(pc + 1, mode1)
          val arg2 = getArgument(pc + 2, mode2)
          pc = if ((arg1 > 0) == condition) arg2 else pc + 3
        }
        case AdjustRelativeBase(mode1) => {
          val arg1 = getArgument(pc + 1, mode1)
          rb += arg1;
          pc += 2
        }
        case Halt() => {
          return Success(_outputs.last)
        }
        case Crash() => {
          return Failure(s"Invalid opcode: ${memory(pc).toString}")
        }
      }
    }
    Failure("Bad memory access")
  }

  private def parseCommand(instruction: Int): Command = {
    val opcode = instruction % 100
    val argMode1 = instruction / 100 % 10
    val argMode2 = instruction / 1000 % 10
    opcode match {
      case 1  => Compute(Add, argMode1, argMode2)
      case 2  => Compute(Multiply, argMode1, argMode2)
      case 3  => Input()
      case 4  => Output(argMode1)
      case 5  => Jump(condition = true, argMode1, argMode2)
      case 6  => Jump(condition = false, argMode1, argMode2)
      case 7  => Compute(LessThan, argMode1, argMode2)
      case 8  => Compute(Equals, argMode1, argMode2)
      case 9  => AdjustRelativeBase(argMode1)
      case 99 => Halt()
      case _  => Crash()
    }
  }

  private def Add: (Int, Int) => Int = (x, y) => x + y

  private def Multiply: (Int, Int) => Int = (x, y) => x * y

  private def LessThan: (Int, Int) => Int = (x, y) => if (x < y) 1 else 0

  private def Equals: (Int, Int) => Int = (x, y) => if (x == y) 1 else 0

  private def getArgument(ptr: Int, mode: Int): Int = {
    mode match {
      case 0 => memory(memory(ptr))
      case 1 => memory(ptr)
      case 2 => memory(rb)
    }
  }

  def outputs: List[Int] = _outputs.toList

  def outputs_=(newOutputs: List[Int]): Unit = {
    _outputs = ListBuffer.from(newOutputs)
  }

}
