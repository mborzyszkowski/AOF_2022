package mb
package cathodeRayTube

import cathodeRayTube.Cpu.xInitialValue
import cathodeRayTube.Instruction.{AddX, Init, Noop}

import scala.annotation.tailrec

object Cpu {
  val xInitialValue = 1
}

case class Cpu(instructionsCyclesMapper: Instruction => Int) {

  def processInstructions(instructions: Seq[Instruction], cyclesToMeasureSignalStrength: Seq[Int]): (Int, Seq[Int]) =
    precessInstructionsInCycles(instructions, CpuCycle(xInitialValue), cyclesToMeasureSignalStrength, Seq())

  @tailrec
  private def precessInstructionsInCycles(instructions: Seq[Instruction], lastCycleInfo: CpuCycle, cyclesToMeasureSignalStrength: Seq[Int], measuredSignalStrengths: Seq[Int]): (Int, Seq[Int]) = {
    val currentCycle = lastCycleInfo.cycleNumber + 1
    val instructionNotFinished = !isInstructionFinished(lastCycleInfo.instructionState)
    val xValue =
      if (instructionNotFinished)
        lastCycleInfo.currentX
      else
        lastCycleInfo.currentX + calculateXChange(lastCycleInfo.instructionState.instruction)
    val updatedMeasuredSignalStrengths = updateMeasuredSignalStrengths(cyclesToMeasureSignalStrength, measuredSignalStrengths, currentCycle, xValue)
    val newCrtValue = if (lastCycleInfo.cycleNumber == 0) 1 else performPrintLogic(lastCycleInfo)

    if (instructionNotFinished)
      precessInstructionsInCycles(instructions, lastCycleInfo.increaseCycle().changeCrt(newCrtValue), cyclesToMeasureSignalStrength, updatedMeasuredSignalStrengths)
    else
      instructions match {
        case instruction :: otherInstructions =>
          val cycleInfo = CpuCycle(InstructionProcessState(instruction, 1), currentCycle, xValue, newCrtValue)
          precessInstructionsInCycles(otherInstructions, cycleInfo, cyclesToMeasureSignalStrength, updatedMeasuredSignalStrengths)
        case _ =>
          println()
          (xValue, updatedMeasuredSignalStrengths)
      }
  }

  private def isInstructionFinished(instructionProcessState: InstructionProcessState): Boolean =
    instructionsCyclesMapper(instructionProcessState.instruction) == instructionProcessState.processedCycles

  private def calculateXChange(instruction: Instruction): Int =
    instruction match {
      case AddX(x) => x
      case Noop() => 0
      case Init() => 0
    }

  private def updateMeasuredSignalStrengths(cyclesToMeasureSignalStrength: Seq[Int], measuredSignalStrengths: Seq[Int], currentCycle: Int, xValue: Int): Seq[Int] =
    if (cyclesToMeasureSignalStrength.contains(currentCycle))
      measuredSignalStrengths :+ currentCycle * xValue
    else
      measuredSignalStrengths

  private def performPrintLogic(lastCycleInfo: CpuCycle): Int = {
    val sprite = getSprite(lastCycleInfo)

    if (lastCycleInfo.currentCrt < 40) {
      print(sprite)
      lastCycleInfo.currentCrt + 1
    } else {
      print(sprite)
      println()
      1
    }
  }

  private def getSprite(lastCycleInfo: CpuCycle): String =
    if (lastCycleInfo.currentCrt >= lastCycleInfo.currentX - 0 && lastCycleInfo.currentCrt <= lastCycleInfo.currentX + 2) "#" else "."
}
