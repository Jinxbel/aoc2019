import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day8Test extends AnyFlatSpec with Matchers {

  type Layer = List[Line]
  type Line = List[Int]
  type Pixel = Int

  "day 8 code" should "divide into pixels" in {

    val width = 25
    val height = 6

    val input = Source.fromResource("Day8Input.txt").getLines().next().toList.map(char => (char + "").toInt)

    val lines: List[Line] = input.sliding(width, width).toList

    val layers: List[Layer] = lines.sliding(height, height).toList

    val minZeroLayer: Layer = layers.minBy(_.flatten.count(_ == 0))

    val numberCount: Pixel => Int = minZeroLayer.flatten.groupBy(i => i).mapValues(_.size)

    println(numberCount(1) * numberCount(2))

    def mergeLayer(upper: Layer, lower: Layer): Layer = {
      upper.zip(lower).map(mergeLine)
    }

    def mergeLine: ((Line, Line)) => Line = { input =>
      val (upper, lower) = input
      upper.zip(lower).map {
        case (0, _) => 0
        case (1, _) => 1
        case (2, x) => x
      }
    }

    val image: Seq[Line] = layers.reduceLeft(mergeLayer)

    def lineToString: Line => String = _.map {
      case 0 => "░"
      case 1 => "█"
      case 0 => " "
    }.mkString(" ")

    println()
    image.map(lineToString).foreach(println)
  }




}
