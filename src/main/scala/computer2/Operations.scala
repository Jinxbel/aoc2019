package computer2

import scala.annotation.tailrec
import scala.io.StdIn

trait Operation {
  def dataset: List[Int]

  def instructionPointer: Int

  def execute(): (List[Int], Int)

  private def opcode = dataset(instructionPointer)

  private def getParam(i: Int) = memoryAccessor(i)

  def firstParam: Int = getParam(0)

  def secondParam: Int = getParam(1)

  private def memoryAccessor(i: Int): Int =
    opcode.toString.dropRight(2).reverse.applyOrElse(i, (i: Int)  => '0') match {
        //position
      case '0' =>
        val address = dataset(instructionPointer + i + 1)
        dataset(address)
      //immediate
      case '1' => dataset(instructionPointer + i + 1)
    }
}



object Pooter2 {
  type Dataset = List[Int]

  def runInstructions(data: String) : Dataset= runInstructions(data.split(",").map(_.toInt).toList)

  @tailrec
  def runInstructions(dataset: Dataset, instructionPointer: Int = 0): Dataset = {
    val operation = Operation.create(dataset, instructionPointer)
    operation match {
      case Exit(_,_) => dataset
      case _ =>
        val (newDataSet, nextOperation) = operation.execute()
        runInstructions(newDataSet, nextOperation)
    }
  }
}

case class OperationFactory(dataset: List[Int], instructionPointer: Int) {
  private def opcode = dataset(instructionPointer)

  private def getParam(i: Int) = memoryAccessor(i)

  def firstParam: Int = getParam(0)

  def secondParam: Int = getParam(1)

  private def memoryAccessor(i: Int): Int =
    opcode.toString.dropRight(2).reverse.applyOrElse(i, (i: Int)  => '0') match {
        //position
      case '0' =>
        val address = dataset(instructionPointer + i + 1)
        dataset(address)
      //immediate
      case '1' => dataset(instructionPointer + i + 1)
    }
}

case class Add2()

object Operation {

  def create(dataset: List[Int], instructionPointer: Int): Operation = {
    val opcode = dataset(instructionPointer)
    opcode.toString.takeRight(2).toInt match {
      case 1 => Add(dataset, instructionPointer)
      case 2 => Multiply(dataset, instructionPointer)
      case 3 => Input(dataset, instructionPointer)
      case 4 => Output(dataset, instructionPointer)
      case 5 => JumpIfTrue(dataset, instructionPointer)
      case 6 => JumpIfFalse(dataset, instructionPointer)
      case 7 => LessThan(dataset, instructionPointer)
      case 8 => Equals(dataset, instructionPointer)
      case 99 => Exit(dataset, instructionPointer)
    }
  }
}

case class Add(dataset: List[Int], instructionPointer: Int) extends Operation {
  private def writeAddress = dataset(instructionPointer + 3)

  def execute(): (List[Int], Int) = {
    (dataset.updated(writeAddress, firstParam + secondParam), instructionPointer + 4)
  }
}

case class Multiply(dataset: List[Int], instructionPointer: Int) extends Operation {
  private def writeAddress = dataset(instructionPointer + 3)

  def execute(): (List[Int], Int) = {
    val newDataSet = dataset.updated(writeAddress, firstParam * secondParam)
    (newDataSet, instructionPointer + 4)
  }
}

case class Input(dataset: List[Int], instructionPointer: Int) extends Operation {
  private def writeAddress = dataset(instructionPointer + 1)

  override def execute(): (List[Int], Int) = {
    val valueToWrite = StdIn.readLine("Input => " + writeAddress + ": ").toInt
    (
      dataset.updated(writeAddress, valueToWrite),
      instructionPointer + 2
    )
  }
}

case class Output(dataset: List[Int], instructionPointer: Int) extends Operation {
  override def execute(): (List[Int], Int) = {
    println("Output: " + firstParam)
    (dataset, instructionPointer + 2)
  }
}

case class JumpIfTrue(dataset: List[Int], instructionPointer: Int) extends Operation {
  private def jumpTo = secondParam

  override def execute(): (List[Int], Int) = {
    if (firstParam != 0) {
      (dataset, jumpTo)
    } else {
      (dataset, instructionPointer + 3)
    }
  }
}

case class JumpIfFalse(dataset: List[Int], instructionPointer: Int) extends Operation {
  private def jumpTo = secondParam

  override def execute(): (List[Int], Int) = {
    if (firstParam == 0) {
      (dataset, jumpTo)
    } else {
      (dataset, instructionPointer + 3)
    }
  }
}

case class LessThan(dataset: List[Int], instructionPointer: Int) extends Operation {
  private def writeAddress = dataset(instructionPointer + 3)

  override def execute(): (List[Int], Int) = {
    if (firstParam < secondParam) {
      (dataset.updated(writeAddress, 1), instructionPointer + 4)
    } else {
      (dataset.updated(writeAddress, 0), instructionPointer + 4)
    }
  }
}

case class Equals(dataset: List[Int], instructionPointer: Int) extends Operation {
  private def writeAddress = dataset(instructionPointer + 3)
  override def execute(): (List[Int], Int) = {
    if (firstParam == secondParam) {
      (dataset.updated(writeAddress, 1), instructionPointer + 4)
    } else {
      (dataset.updated(writeAddress, 0), instructionPointer + 4)
    }
  }
}

case class Exit(dataset: List[Int], instructionPointer: Int) extends Operation {
  override def execute(): (List[Int], Int) = ???
}