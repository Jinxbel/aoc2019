import scala.annotation.tailrec

object D2IntOp {
  def main(args: Array[String]): Unit = {
    val input: List[Int] = args(0).split(',').map(_.toInt).toList

    @tailrec
    def processStep(dataset: List[Int], stepNumber:Int) : List[Int] = {
      val opt = dataset.drop(stepNumber * 4)
      opt match {
        case 1 :: left :: right :: write :: _ => processStep(dataset.updated(write, dataset(left) + dataset(right)), stepNumber +1)
        case 2 :: left :: right :: write :: _ => processStep(dataset.updated(write, dataset(left) * dataset(right)), stepNumber +1)
        case 99 :: _ => dataset
      }
    }

    val results: Stream[(Int,Int)] = for {
      x <- Stream.range(0,100)
      y <- Stream.range(0,100)
      dataset = processStep(input.updated(1, x).updated(2,y), 0)
      if(dataset.head == 19690720)
    } yield (x,y)

//    println(processStep(input, 0).mkString(","))
    println(results.head)

  }

}
