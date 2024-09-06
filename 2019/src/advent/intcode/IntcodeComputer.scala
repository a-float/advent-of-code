package advent.intcode

import advent.intcode.IntcodeComputer.Program

import scala.collection.mutable.ListBuffer
import scala.io.Source

object IntcodeComputer {
  private type Program = Array[Long]

  def loadProgramFromFile(filename: String): IntcodeComputer =
    new IntcodeComputer(
      readProgramFromFile(
        Source
          .fromResource(filename)
      )
    )

  def readProgramFromFile(src: Source): Program = src.mkString
    .filterNot(_.isWhitespace)
    .trim
    .split(',')
    .map(_.toLong)

  def loadProgram(src: Source): IntcodeComputer =
    new IntcodeComputer(readProgramFromFile(src))
}

class IntcodeComputer(val program: Array[Long]) extends Cloneable {
  private var pc: Long = 0 // program counter
  private var rb: Long = 0 // relative base
  private var memory: Program = program.clone()
  private var _outputs: ListBuffer[Long] = ListBuffer.empty[Long]

  override def clone(): IntcodeComputer = {
    val computer = new IntcodeComputer(memory)
    computer.pc = pc
    computer.rb = rb
    computer
  }

  def run: List[Long] => Result = execute(waitForInputs = true)(_)

  def execute(waitForInputs: Boolean = false)(
      inputs: List[Long] = List.empty
  ): Result = {
    var inputIdx = 0
    while (0 <= pc && pc < memory.length) {
      parseCommand(readMemory(pc)) match {
        case Compute(op, mode1, mode2, mode3) => {
          val arg1 = getArgument(pc + 1, mode1)
          val arg2 = getArgument(pc + 2, mode2)
          val dest = getArgument(pc + 3, mode3, isLiteral = true)
          setMemory(dest, op(arg1, arg2))
          pc += 4
        }
        case Input(mode1) => {
          if (inputIdx >= inputs.size) {
            if (waitForInputs) {
              return Continue(this)
            } else {
              return Failure("Out of inputs")
            }
          }
          val dest = getArgument(pc + 1, mode1, isLiteral = true)
          setMemory(dest, inputs(inputIdx))
          inputIdx += 1
          pc += 2
        }
        case Output(mode1) => {
          val arg = getArgument(pc + 1, mode1)
          _outputs += arg
          pc += 2
        }
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
          return if (outputs.nonEmpty) Success(_outputs.last)
          else Failure(s"Halt with no output")
        }
        case Crash() => {
          return Failure(s"Invalid opcode: ${readMemory(pc).toString}")
        }
      }
    }
    Failure("Bad memory access")
  }

  private def readMemory(idx: Long): Long =
    if (idx > memory.length - 1) 0 else memory(idx.toInt)

  private def setMemory(idx: Long, value: Long): Unit = {
    if (idx > memory.length - 1) {
      val newMemory = Array.fill(idx.toInt + 1) { 0L }
      memory.copyToArray(newMemory)
      memory = newMemory
    }
    memory(idx.toInt) = value
  }

  private def parseCommand(instruction: Long): Command = {
    val opcode = instruction % 100
    val mode1 = instruction / 100 % 10
    val mode2 = instruction / 1000 % 10
    val mode3 = instruction / 10000 % 10
    opcode match {
      case 1  => Compute((x, y) => x + y, mode1, mode2, mode3)
      case 2  => Compute((x, y) => x * y, mode1, mode2, mode3)
      case 3  => Input(mode1)
      case 4  => Output(mode1)
      case 5  => Jump(condition = true, mode1, mode2)
      case 6  => Jump(condition = false, mode1, mode2)
      case 7  => Compute((x, y) => if (x < y) 1 else 0, mode1, mode2, mode3)
      case 8  => Compute((x, y) => if (x == y) 1 else 0, mode1, mode2, mode3)
      case 9  => AdjustRelativeBase(mode1)
      case 99 => Halt()
      case _  => Crash()
    }
  }

  private def getArgument(
      ptr: Long,
      mode: Long,
      isLiteral: Boolean = false
  ): Long = {
    if (isLiteral)
      mode match {
        case 0 | 1 => readMemory(ptr)
        case 2     => rb + readMemory(ptr)
      }
    else
      mode match {
        case 0 => readMemory(readMemory(ptr))
        case 1 => readMemory(ptr)
        case 2 => readMemory(rb + readMemory(ptr))
      }
  }

  def clearOutputs(): Unit = _outputs.clear()

  def outputs: List[Long] = _outputs.toList

  def outputs_=(newOutputs: List[Long]): Unit = {
    _outputs = ListBuffer.from(newOutputs)
  }
}
