import computer.IntCodeComputer

import scala.annotation.tailrec
import scala.io.StdIn



object D2IntOp {
  def main(args: Array[String]): Unit = {
    val input: List[Int] = args(0).split(',').map(_.toInt).toList


    val results: Stream[(Int, Int)] = for {
      x <- Stream.range(0, 100)
      y <- Stream.range(0, 100)
      dataset = IntCodeComputer.processStep(input.updated(1, x).updated(2, y), 0)
      if (dataset.head == 19690720)
    } yield (x, y)

    //    println(processStep(input, 0).mkString(","))
    println(results.head)

  }

}
