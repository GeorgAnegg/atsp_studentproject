package ch.ethz.math.ifor.atsp.BranchAndCut.pricingSchemes

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode
import ch.ethz.math.ifor.atsp.Site

object APPricing extends PricingScheme {
  def updateColumns(branchNode: BranchNode): Map[Site, Map[Site, Double]] = {

    branchNode.costsMap
  }

}
