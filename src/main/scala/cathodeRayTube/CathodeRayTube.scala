package mb
package cathodeRayTube

import cathodeRayTube.Instruction.{AddX, Init, Noop}

object CathodeRayTube extends App {

  val filePath = "src/main/scala/cathodeRayTube/input.txt"

  val instructions = Instruction.parseFromFile(filePath)

  val cyclesPerInstructionV1: Instruction => Int = {
    case AddX(_) => 2
    case Noop() => 1
    case Init() => 0
  }

  val cpuV1 = Cpu(cyclesPerInstructionV1)
  val cyclesToMeasureSignalStrengthV1 = Seq(20, 60, 100, 140, 180, 220)

  val (finalValueX1, signalStrengthMeasurementV1) = cpuV1.processInstructions(instructions, cyclesToMeasureSignalStrengthV1)

  println(instructions)
  println(s"x: $finalValueX1, measurementsSum: ${signalStrengthMeasurementV1.sum}, measurements: ${signalStrengthMeasurementV1}")
  println()
}
