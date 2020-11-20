package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers

import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, IsLeafNode, LowerBound}

trait LowerBoundSolver {

  //TODO: figure out of there is a better way to do this
  def compute (branchNode: BranchNode) : (LowerBound, IsLeafNode)

}
