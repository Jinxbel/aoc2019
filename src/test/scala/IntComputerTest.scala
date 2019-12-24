import computer2.PooterState
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import scala.io.Source

class IntComputerTest extends AnyFlatSpec with Matchers {

  "Pooter" should "Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not)." in {
    val initialState = PooterState("3,9,8,9,10,9,4,9,99,-1,8")

    initialState.runWithInput(LazyList(8)).output.head shouldBe 1
    initialState.runWithInput(LazyList(7)).output.head shouldBe 0
  }

  it should "Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not)." in {
    val initialState = PooterState("3,9,7,9,10,9,4,9,99,-1,8")

    initialState.runWithInput(LazyList(7)).output.head shouldBe 1
    initialState.runWithInput(LazyList(8)).output.head shouldBe 0
  }

  it should "Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not)." in {
    val initialState = PooterState("3,3,1108,-1,8,3,4,3,99")

    initialState.runWithInput(LazyList(8)).output.head shouldBe 1
    initialState.runWithInput(LazyList(7)).output.head shouldBe 0
  }

  it should "Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not)" in {
    val initialState = PooterState("3,3,1107,-1,8,3,4,3,99")

    initialState.runWithInput(LazyList(7)).output.head shouldBe 1
    initialState.runWithInput(LazyList(8)).output.head shouldBe 0
  }

  it should "take an input, then output 0 if the input was zero or 1 if the input was non-zero with position Mode" in {
    val initialState = PooterState("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")
    initialState.runWithInput(LazyList(0)).output.head shouldBe 0
    initialState.runWithInput(LazyList(1)).output.head shouldBe 1
  }

  it should "take an input, then output 0 if the input was zero or 1 if the input was non-zero with immediate Mode" in {
    val initialState = PooterState("3,3,1105,-1,9,1101,0,0,12,4,12,99,1")
    initialState.runWithInput(LazyList(0)).output.head shouldBe 0
    initialState.runWithInput(LazyList(1)).output.head shouldBe 1
  }


  it should "run day 5" in {
    val input = Source.fromResource("Day5Input.txt").getLines().next()

    PooterState(input).runWithInput(LazyList(1)).output.takeRight(1).head shouldBe 15259545
    PooterState(input).runWithInput(LazyList(5)).output.takeRight(1).head shouldBe 7616021
  }

  it should "run example amplifier 1" in {
    val results = runAmplifiersWithSoftware("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0", List.range(0, 5))
    val (input, output) = results.maxBy(_._2)

    input shouldBe List(4, 3, 2, 1, 0)
    output shouldBe 43210
  }

  it should "run example amplifier 2" in {
    val results = runAmplifiersWithSoftware("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0", List.range(0, 5))
    val (input, output) = results.maxBy(_._2)

    input shouldBe List(0, 1, 2, 3, 4)
    output shouldBe 54321
  }

  it should "run example amplifier 3" in {
    val results = runAmplifiersWithSoftware("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0", List.range(0, 5))
    val (input, output) = results.maxBy(_._2)

    input shouldBe List(1, 0, 4, 3, 2)
    output shouldBe 65210
  }


  it should "run day 7 part 1" in {
    val ampControlSoftware = Source.fromResource("Day7Input.txt").getLines().next()
    val results = runAmplifiersWithSoftware(ampControlSoftware, List.range(0, 5))

    val outputMax = results.maxBy(_._2)

    outputMax._2 shouldBe 47064
  }


  it should "run day 7 part 2 example 1" in {
    val amplifier = PooterState("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")

    val possiblePhases = List.range(5, 10).permutations.toList
    val results = possiblePhases.map(phases => {
      val amps = phases.map(amplifier.withInput)
      (phases, runSetWithInput(amps, input = LazyList(0)))
    })

    val (phases, result) = results.maxBy(_._2)

    phases shouldBe List(9, 8, 7, 6, 5)
    result shouldBe 139629729
  }

  it should "run day 7 part 2 example 2" in {
    val amplifier = PooterState("3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")

    val possiblePhases = List.range(5, 10).permutations.toList
    val results = possiblePhases.map(phases => {
      val amps = phases.map(amplifier.withInput)
      (phases, runSetWithInput(amps, input = LazyList(0)))
    })

    val (phases, result) = results.maxBy(_._2)

    phases shouldBe List(9,7,8,5,6)
    result shouldBe 18216
  }

  it should "run day 7 part 2" in {
    val amplifier = PooterState(Source.fromResource("Day7Input.txt").getLines().next())

    val possiblePhases = List.range(5, 10).permutations.toList
    val results = possiblePhases.map(phases => {
      val amps = phases.map(amplifier.withInput)
      (phases, runSetWithInput(amps, input = LazyList(0)))
    })

    val (phases, result) = results.maxBy(_._2)

    result shouldBe 4248984
  }

  @tailrec
  final def runSetWithInput(amps: List[PooterState], input: LazyList[Int], useInputForAmp: Int = 0): Int = {
    val ampResult = amps(useInputForAmp).runWithInput(input)

    if (amps.size - 1 == useInputForAmp && ampResult.shouldExit) {
      ampResult.output.last
    } else {
      runSetWithInput(
        amps.updated(useInputForAmp, ampResult.stripOutput),
        LazyList.from(ampResult.output),
        if (useInputForAmp == amps.size - 1) 0 else useInputForAmp + 1
      )
    }
  }

  private def runAmplifiersWithSoftware(ampControlSoftware: String, phaseSettings: List[Int]): List[(List[Int], Int)] = {
    val amplifier = PooterState(ampControlSoftware)

    val possiblePhases = phaseSettings.permutations.toList
    val inputSignal = 0

    def runWithPhases(phases: List[Int]): Int = {
      phases
        .map(amplifier.withInput)
        .foldLeft(inputSignal) {
          (input: Int, amp: PooterState) => amp.runWithInput(input).output.head
        }
    }

    val outputValues = possiblePhases.map(phases =>
      (phases, runWithPhases(phases))
    )

    outputValues

  }
}
