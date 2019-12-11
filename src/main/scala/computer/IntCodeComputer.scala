package computer

import scala.annotation.tailrec

object IntCodeComputer {

  def process(dataset: List[Int]): Unit = processStep(dataset, 0)

  def toMode(head: Int) : Stream[ArgumentMode] = {
    Stream.concat(head.toString.dropRight(2).reverse
        .map {
        case '0' => PositionMode()
        case '1' => ImmediateMode()
        },
      Stream.continually(PositionMode()))
  }

  @tailrec
  def processStep(dataset: List[Int], nextInstructionAt: Int): List[Int] = {

    val currentOperations = dataset.drop(nextInstructionAt)
    val operand: Operand = Operand(currentOperations.head)
    val modes = toMode(currentOperations.head)

    operand match {
      case Exit => dataset
      case operand: Operand =>
        val (newDataset,nextInstruction) = operand.execute(dataset, nextInstructionAt, modes)
        processStep(newDataset, nextInstruction)
    }
  }
}
