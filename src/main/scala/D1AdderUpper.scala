import java.math.MathContext

import scala.annotation.tailrec

object AdderUpper {
  def main(args: Array[String]): Unit = {
    println(args.map(_.toInt).map(Module).map(_.requiredLaunchFuel).sum)
  }
}

case class Module(mass: Int) {
  def requiredLaunchFuel: Int = FuelCalculator.calculateTotalFuelFor(mass)


}

object FuelCalculator {
  def calculateFuelRequiredmentForMass(mass: Int): Int = {
    (BigDecimal(mass) / 3)
      .setScale(0, BigDecimal.RoundingMode.FLOOR).intValue - 2
  }

  def calculateTotalFuelFor(mass:Int): Int = {
    FuelCalculator.calculateFuelRequiredmentForMass(mass) match {
      case x:Int if x <= 0 => 0
      case x:Int => x + calculateTotalFuelFor(x)
    }
  }

}
