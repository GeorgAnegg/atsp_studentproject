package ch.ethz.math.ifor.atsp.BranchAndCut.cuttingPlanes

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode
import com.google.ortools.linearsolver.{MPConstraint, MPObjective, MPSolver, MPVariable}

object OddCAT extends CuttingPlane {
  def findCuts(branchNode:BranchNode):List[MPConstraint] = ???

}
