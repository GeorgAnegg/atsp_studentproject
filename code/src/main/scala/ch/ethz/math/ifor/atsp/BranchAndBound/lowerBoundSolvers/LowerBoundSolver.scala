package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers

import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, LowerBound}
import ch.ethz.math.ifor.atsp.Site

trait LowerBoundSolver {

  //TODO: figure out of there is a better way to do this
  def compute (branchNode: BranchNode) : (Map[Site, Map[Site, Boolean]],Map[Site, Map[Site, Double]])

  def computeLB (branchNode: BranchNode) : LowerBound

}
