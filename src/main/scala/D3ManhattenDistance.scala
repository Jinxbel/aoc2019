import com.sun.javafx.geom.{Line2D, Point2D}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.immutable.Queue

object D3ManhattenDistance {

  def main(args: Array[String]): Unit = {
    val result = calculateMinManhattenDistance(Array("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66," +
      "U55,R34,D71,R55,D58,R83"))
    println(result)
    assert(result._1 .abs + result._2.abs == 159)

    val minDistance: (Int, Int) = calculateMinManhattenDistance(args)
    println(minDistance)
    println(ManhattenDistance.calculate(minDistance))

    val minWireLength = calculateMinWireDistance(args)
    println(minWireLength)

  }

  private def calculateMinWireDistance(args: Array[String]) = {
    val wire1positions = Wire(args.head.split(',').toList).toPositions().reverse
    val wire2positions = Wire(args(1).split(',').toList).toPositions().reverse

    val intersections = wire1positions.intersect(wire2positions).filter(_ != (0,0))
    val minDistance = intersections.map(intersection => {
      (wire1positions.indexOf(intersection) + wire2positions.indexOf(intersection), intersection)
    })
      .minBy(_._1)
    minDistance._1
  }

  private def calculateMinManhattenDistance(args: Array[String]) = {
    val wire1 = Wire(args.head.split(',').toList).toPositions().reverse.tail
    val wire2 = Wire(args(1).split(',').toList).toPositions().reverse.tail


    val intersections = wire1.intersect(wire2)
    val minDistance = intersections
      .minBy(ManhattenDistance.calculate)
    minDistance
  }
}


object ManhattenDistance {
  def calculate(coords: (Int, Int)) : Int = coords._1.abs + coords._2.abs
}

case class Wire(path: List[String]) {
  def toPositions() : List[(Int,Int)] = {

    val steps: Stream[Step] = path.toStream.flatMap(step => {
      step.head match {
        case 'R' => Stream.fill(step.tail.toInt)(R())
        case 'L' => Stream.fill(step.tail.toInt)(L())
        case 'U' => Stream.fill(step.tail.toInt)(U())
        case 'D' => Stream.fill(step.tail.toInt)(D())
      }
    })

    @tailrec
    def doSteps(steps: Stream[Step], result: List[(Int, Int)]): List[(Int, Int)] = {
      steps match {
        case Stream.Empty => result
        case step #:: tail => doSteps(tail, step.step(result.head) :: result)
      }
    }

    doSteps(steps, List((0,0)))
  }
}

trait Step {
  def step(startPosition: (Int,Int)): (Int, Int)
}
case class R() extends Step {
  def step(startPosition: (Int,Int)): (Int, Int) = (startPosition._1 + 1, startPosition._2)
}
case class L() extends Step {
  def step(startPosition: (Int,Int)): (Int, Int) = (startPosition._1 - 1, startPosition._2)
}
case class U() extends Step {
  def step(startPosition: (Int,Int)): (Int, Int) = (startPosition._1, startPosition._2 + 1)
}
case class D() extends Step {
  def step(startPosition: (Int,Int)): (Int, Int) = (startPosition._1, startPosition._2 - 1)
}