package computer

import scala.io.StdIn

trait Operand{
  def execute(dataset:List[Int], nextOperationAt: Int, modes: Seq[ArgumentMode]): (List[Int], Int)
}
object Operand {
  def apply(int: Int): Operand = {
    int.toString.takeRight(2).toInt match {
      case 1 => Add
      case 2 => Multiply
      case 3 => Input
      case 4 => Output
      case 5 => JumpIfTrue
      case 6 => JumpIfFalse
      case 7 => LessThan
      case 8 => Equals
      case 94 => Exit
      case 99 => Exit
    }
  }
}

case object JumpIfTrue extends Operand {
  override def execute(dataset: List[Int], nextOperationAt: Int, modes: Seq[ArgumentMode]):  (List[Int], Int) = {
    val currentOperations = dataset.drop(nextOperationAt)
    val read1 :: jumpTo :: Nil = currentOperations.tail.take(2)
    val param1 = modes.head.get(dataset, read1)
    val next =if (param1 != 0) jumpTo else nextOperationAt + 3
    (dataset, next)
  }
}

case object JumpIfFalse extends Operand {
  override def execute(dataset: List[Int], nextOperationAt: Int, modes: Seq[ArgumentMode]):  (List[Int], Int) = {
    val currentOperations = dataset.drop(nextOperationAt)
    val read1 :: jumpTo :: Nil = currentOperations.tail.take(2)
    val param1 = modes.head.get(dataset, read1)
    val next = if (param1 == 0) jumpTo else nextOperationAt + 3
    (dataset, next)
  }
}

case object LessThan extends Operand {
  override def execute(dataset: List[Int], nextOperationAt: Int, modes: Seq[ArgumentMode]): (List[Int], Int) = {
    val currentOperations = dataset.drop(nextOperationAt)
    val read1 :: read2 :: writeLocation :: Nil = currentOperations.tail.take(3)
    val param1 = modes(0).get(dataset, read1)
    val param2 = modes(1).get(dataset, read2)
    (dataset.updated(writeLocation, if(param1 < param2) 1 else 0), nextOperationAt + 4)
  }
}

case object Equals extends Operand {
  override def execute(dataset: List[Int], nextOperationAt: Int, modes: Seq[ArgumentMode]): (List[Int], Int) = {
    val currentOperations = dataset.drop(nextOperationAt)
    val read1 :: read2 :: writeLocation :: Nil = currentOperations.tail.take(3)
    val param1: Int = modes(0).get(dataset, read1)
    val param2: Int = modes(1).get(dataset, read2)
    (dataset.updated(writeLocation, if(param1 == param2) 1 else 0), nextOperationAt + 4)
  }
}

case object Add extends Operand {
  override def execute(dataset:List[Int], nextOperationAt: Int,  modes: Seq[ArgumentMode])
  :  (List[Int], Int) = {
    val currentOperations = dataset.drop(nextOperationAt)
    val read1 :: read2 :: writeLocation :: Nil = currentOperations.tail.take(3)
    val param1 = modes(0).get(dataset, read1)
    val param2 = modes(1).get(dataset, read2)
    (dataset.updated(writeLocation, param1 + param2), nextOperationAt + 4)
  }
}
case object Multiply extends Operand {
  override def execute(dataset:List[Int], nextOperationAt: Int, modes: Seq[ArgumentMode]):  (List[Int], Int) = {
    val currentOperations = dataset.drop(nextOperationAt)
    val read1 :: read2 :: writeLocation :: Nil = currentOperations.tail.take(3)
    val param1 = modes(0).get(dataset, read1)
    val param2 = modes(1).get(dataset, read2)
    (dataset.updated(writeLocation, param1 * param2), nextOperationAt + 4)
  }
}
case object Input extends Operand {
  override def execute(dataset:List[Int], nextOperationAt: Int, modes: Seq[ArgumentMode])
  :  (List[Int], Int) = {
    val currentOperations = dataset.drop(nextOperationAt)
    val writeLocation :: Nil = currentOperations.tail.take(1)
    (dataset.updated(writeLocation, StdIn.readLine("Input: ").toInt), nextOperationAt + 2)
  }
}
case object Output extends Operand {
  override def execute(dataset:List[Int], nextOperationAt: Int, modes: Seq[ArgumentMode]):  (List[Int], Int) = {
    val currentOperations = dataset.drop(nextOperationAt)
    val read1 :: Nil = currentOperations.tail.take(1)
    val param1 = modes(0).get(dataset, read1)
    println("Output: " + param1)
    (dataset, nextOperationAt + 2)
  }
}
case object Exit extends Operand {
  override def execute(dataset: List[Int], nextOperationAt: Int, modes: Seq[ArgumentMode])
  :  (List[Int], Int) = throw new RuntimeException("should not be executed")

}

