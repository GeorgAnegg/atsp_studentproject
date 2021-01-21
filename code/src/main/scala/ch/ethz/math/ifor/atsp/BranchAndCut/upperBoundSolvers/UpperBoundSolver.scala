package ch.ethz.math.ifor.atsp.BranchAndCut.upperBoundSolvers

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode

trait UpperBoundSolver {
  def computeUpperBound(branchNode: BranchNode): Double

}
