package ch.ethz.math.ifor.atsp.BranchAndCut.pricingSchemes

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode
import ch.ethz.math.ifor.atsp.Site

trait PricingScheme {
  def updateColumns(branchNode: BranchNode): Map[Site, Map[Site, Double]]

}
