package computer2

import computer2.Pooter2.Dataset

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.StdIn

trait Operation {
  def state: PooterState
  def dataset: List[Int] = state.dataset
  def instructionPointer: Int = state.instructionPointer
  def inputs = state.inputs
  def output = state.output
  def execute(): PooterState

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

object PooterState {
  def apply(dataset:String) : PooterState = PooterState(
    dataset = dataset.split(',').map(_.toInt).toList
  )
}
case class PooterState(dataset: Dataset,
                       instructionPointer: Int = 0,
                       inputs: LazyList[Int] = LazyList.empty,
                       output: Queue[Int] = Queue.empty
                      ) {
  def stripOutput: PooterState = this.copy(output = Queue.empty)

  def withInput(extraInputs: LazyList[Int]): PooterState = this.copy(inputs = inputs.appendedAll(extraInputs))
  def withInput(extraInput: Int): PooterState = this.copy(inputs = inputs.appended(extraInput))

  def runWithInput(value: LazyList[Int]) = this.withInput(value).run
  def runWithInput(value: Int) = this.withInput(value).run

  lazy val operation = Operation.create(this)

  def shouldExit: Boolean = operation.isInstanceOf[Exit]
  def isWaitingForInput: Boolean = operation.isInstanceOf[Input] && inputs.isEmpty

  def tick: PooterState = operation.execute()

  @tailrec
  final def run: PooterState = if (shouldExit || isWaitingForInput) this else tick.run

}

object Pooter2 {
  type Dataset = List[Int]

  def runInstructions(data: String) : Dataset= runInstructions(data.split(",").map(_.toInt).toList)

  def runInstructions(dataset: Dataset, instructionPointer: Int = 0) : Dataset = {
    val newState = PooterState(
      dataset,
      instructionPointer,
      LazyList.continually(StdIn.readLine("Input: ").toInt),
      Queue.empty
    ).run
    newState.output.foreach(println)
    newState.dataset
  }

//  @tailrec
//  def runInstructions(dataset: Dataset, instructionPointer: Int = 0): Dataset = {
//    val operation = Operation.create(dataset, instructionPointer)
//    operation match {
//      case Exit(_,_) => dataset
//      case _ =>
//        val (newDataSet, nextOperation) = operation.execute()
//        runInstructions(newDataSet, nextOperation)
//    }
//  }
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

  def create(state: PooterState): Operation = {
    val opcode = state.dataset(state.instructionPointer)
    opcode.toString.takeRight(2).toInt match {
      case 1 => Add(state)
      case 2 => Multiply(state)
      case 3 => Input(state)
      case 4 => Output(state)
      case 5 => JumpIfTrue(state)
      case 6 => JumpIfFalse(state)
      case 7 => LessThan(state)
      case 8 => Equals(state)
      case 99 => Exit(state)
    }
  }
}

case class Add(state:PooterState) extends Operation {
  private def writeAddress = dataset(instructionPointer + 3)

  def execute(): PooterState = {
    state.copy(
      dataset = dataset.updated(writeAddress, firstParam + secondParam),
      instructionPointer = instructionPointer + 4
    )
  }
}

case class Multiply(state:PooterState) extends Operation {
  private def writeAddress = dataset(instructionPointer + 3)

  def execute(): PooterState =
    state.copy(
      dataset = dataset.updated(writeAddress, firstParam * secondParam),
      instructionPointer = instructionPointer + 4
    )
}

case class Input(state:PooterState) extends Operation {
  private def writeAddress = dataset(instructionPointer + 1)

  override def execute(): PooterState =
    state.copy(
      dataset = dataset.updated(writeAddress, inputs.head),
      instructionPointer = instructionPointer + 2,
      inputs =  inputs.tail
    )
}

case class Output(state:PooterState) extends Operation {
  override def execute(): PooterState = state.copy(
    instructionPointer = instructionPointer + 2,
    output = output.enqueue(firstParam)
  )
}

case class JumpIfTrue(state:PooterState) extends Operation {
  private def jumpTo = secondParam

  override def execute(): PooterState = {
    if (firstParam != 0) {
      state.copy(instructionPointer =  jumpTo)
    } else {
      state.copy(instructionPointer = instructionPointer + 3)
    }
  }
}

case class JumpIfFalse(state:PooterState) extends Operation {
  private def jumpTo = secondParam

  override def execute(): PooterState = {
    if (firstParam == 0) {
      state.copy(instructionPointer =  jumpTo)
    } else {
      state.copy(instructionPointer =  instructionPointer + 3)
    }
  }
}

case class LessThan(state:PooterState) extends Operation {
  private def writeAddress = dataset(instructionPointer + 3)

  override def execute(): PooterState = {
    if (firstParam < secondParam) {
      state.copy(
        dataset = dataset.updated(writeAddress, 1),
        instructionPointer = instructionPointer + 4
      )
    } else {
      state.copy(
        dataset = dataset.updated(writeAddress, 0),
        instructionPointer = instructionPointer + 4
      )
    }
  }
}

case class Equals(state:PooterState) extends Operation {
  private def writeAddress = dataset(instructionPointer + 3)
  override def execute(): PooterState = {
    if (firstParam == secondParam) {
      state.copy(
        dataset = dataset.updated(writeAddress, 1),
        instructionPointer = instructionPointer + 4
      )
    } else {
      state.copy(
        dataset = dataset.updated(writeAddress, 0),
        instructionPointer = instructionPointer + 4
      )
    }
  }
}

case class Exit(state:PooterState) extends Operation {
  override def execute(): PooterState = ???
}