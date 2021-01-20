package ch.ethz.math.ifor.atsp.BranchAndBound.pricingSchemes

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchNode
import ch.ethz.math.ifor.atsp.Site

trait PricingSchemes {
  def solutionPricing(branchNode: BranchNode): Map[Site, Map[Site, Double]]

}
