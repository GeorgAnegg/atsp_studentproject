package ch.ethz.math.ifor.atsp

import ch.ethz.math.ifor.atsp.BranchAndCut.branchingSchemes.naiveBranching
import ch.ethz.math.ifor.atsp.BranchAndCut.cuttingPlanes.OddCAT
import ch.ethz.math.ifor.atsp.BranchAndCut.cuttingPlanes.MinCut
import ch.ethz.math.ifor.atsp.BranchAndCut.linearProgrmmingSolvers.ORToolsLP
import ch.ethz.math.ifor.atsp.BranchAndCut.pricingSchemes.APPricing
import ch.ethz.math.ifor.atsp.BranchAndCut.upperBoundSolvers.naiveUpperBound


package object BranchAndCut {
  val linearProgrammingSolver = ORToolsLP
  val pricingScheme = APPricing
  val branchingScheme = naiveBranching
  val upperBoundSolver = naiveUpperBound
  val cuttingPlane = MinCut


}
