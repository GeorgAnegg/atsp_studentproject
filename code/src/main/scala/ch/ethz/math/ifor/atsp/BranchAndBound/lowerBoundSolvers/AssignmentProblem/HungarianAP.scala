package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.AssignmentProblem
import ch.ethz.math.ifor.atsp.Site
import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, LowerBound}
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.LowerBoundSolver
object HungarianAP extends LowerBoundSolver{

  def compute(branchNode: BranchNode): Map[Site, Map[Site, Boolean]]  = {

    val numSites = branchNode.sitesStatus.size
    val inputN = branchNode.inputNode
    val resultArray = Array.ofDim[Boolean](numSites, numSites)

    // TODO: implement the O(n^3) version Hungarian Method




    val resultBoolean : Map[Site, Map[Site, Boolean]] = inputN.sites.zip(resultArray).map{case (site, distRow) =>
      site -> inputN.sites.zip(distRow).toMap}.toMap
    resultBoolean

  }

  // not used
  def computeLB (branchNode: BranchNode) : LowerBound = {
    0.0
  }


}
