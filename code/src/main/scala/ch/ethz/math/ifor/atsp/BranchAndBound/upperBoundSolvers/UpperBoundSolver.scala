package ch.ethz.math.ifor.atsp.BranchAndBound.upperBoundSolvers
import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, LowerBound}
import ch.ethz.math.ifor.atsp.Site
trait UpperBoundSolver {
  def computeUpperBound(branchNode: BranchNode): Double

}
