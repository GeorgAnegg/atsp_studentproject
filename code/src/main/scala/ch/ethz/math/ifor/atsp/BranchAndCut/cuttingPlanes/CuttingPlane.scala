package ch.ethz.math.ifor.atsp.BranchAndCut.cuttingPlanes

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode
import com.google.ortools.linearsolver.{MPConstraint, MPObjective, MPSolver, MPVariable}

trait CuttingPlane {
  def findCuts(branchNode:BranchNode, globalCuts:List[(Map[MPVariable,Double],Double)]):List[(Map[MPVariable,Double],Double)]


}
