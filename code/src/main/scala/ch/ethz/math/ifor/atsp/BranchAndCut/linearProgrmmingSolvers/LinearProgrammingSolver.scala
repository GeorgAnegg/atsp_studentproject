package ch.ethz.math.ifor.atsp.BranchAndCut.linearProgrmmingSolvers

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode
import ch.ethz.math.ifor.atsp.Site

trait LinearProgrammingSolver {
  def findSolution(branchNode: BranchNode): Map[Site, Map[Site, Double]]

}
