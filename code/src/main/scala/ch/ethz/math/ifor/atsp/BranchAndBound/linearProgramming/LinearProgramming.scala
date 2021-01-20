package ch.ethz.math.ifor.atsp.BranchAndBound.linearProgramming

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchNode
import ch.ethz.math.ifor.atsp.{Input, Output, Site}

trait LinearProgramming {

  def findSolution(branchNode: BranchNode): Map[Site, Map[Site, Double]]

}
