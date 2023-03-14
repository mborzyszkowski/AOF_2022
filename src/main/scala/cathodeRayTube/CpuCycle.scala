package mb
package cathodeRayTube

import cathodeRayTube.Instruction.Init

case class CpuCycle(instructionState: InstructionProcessState, cycleNumber: Int, currentX: Int, currentCrt: Int) {

  def increaseCycle(): CpuCycle =
    CpuCycle(InstructionProcessState(instructionState.instruction, instructionState.processedCycles + 1), cycleNumber + 1, currentX, currentCrt)

  def changeCrt(crtValue: Int): CpuCycle =
    CpuCycle(instructionState, cycleNumber, currentX, crtValue)
}

object CpuCycle {
  def apply(xInitialValue: Int): CpuCycle = CpuCycle(InstructionProcessState(Init(), 0), 0, xInitialValue, 1)
}