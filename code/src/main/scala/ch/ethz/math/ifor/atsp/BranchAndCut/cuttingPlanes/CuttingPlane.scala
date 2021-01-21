package ch.ethz.math.ifor.atsp.BranchAndCut.cuttingPlanes

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode
import optimus.optimization.model.MPConstraint

trait CuttingPlane {
  def findCuts(branchNode:BranchNode):List[MPConstraint]

}
