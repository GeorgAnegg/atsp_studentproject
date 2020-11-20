package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers

import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, IsLeafNode, LowerBound}
import com.google.ortools.linearsolver.MPVariable

trait LowerBoundSolver {

  //TODO: figure out of there is a better way to do this
  def compute (branchNode: BranchNode) : Map[MPVariable, Boolean]

}
