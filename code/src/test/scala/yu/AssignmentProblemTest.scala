package yu

import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.AssignmentProblem.ORToolsIP
import ch.ethz.math.ifor.atsp.Input
import ch.ethz.math.ifor.atsp.{Input, Site, Tour}
import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, lowerBoundSolver}


object AssignmentProblemTest extends App {
  //val input = Input.toyExample
  /*
  val input = Input.toyExample
  val numSites: Int = input.sites.length
  val initAssignmentArray: Array[Array[Option[Boolean]]] = Array.ofDim[Option[Boolean]](numSites, numSites)
  val initAssignmentMap: Map[Site, Map[Site, Option[Boolean]]] = input.sites.zip(initAssignmentArray).map{case (site, distRow) =>
    site -> input.sites.zip(distRow).toMap}.toMap
  val rootNode: BranchNode = new BranchNode(input, initAssignmentMap)
  val lowerBoundSolve: Map[Site, Map[Site, Boolean]] = lowerBoundSolver.compute(branchNode = rootNode)._1

   */


}
