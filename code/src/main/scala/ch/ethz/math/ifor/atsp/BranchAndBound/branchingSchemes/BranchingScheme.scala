package ch.ethz.math.ifor.atsp.BranchAndBound.branchingSchemes

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchNode
import ch.ethz.math.ifor.atsp.{Input, Output}

trait BranchingScheme {

  def listChildren(branchNode: BranchNode): List[BranchNode]

}
