package ch.ethz.math.ifor.atsp.BranchAndCut.branchingSchemes

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode

trait BranchingSchemes {
  def listChildren(branchNode: BranchNode): List[BranchNode]
}
