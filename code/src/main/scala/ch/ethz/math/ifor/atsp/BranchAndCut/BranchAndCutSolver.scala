package ch.ethz.math.ifor.atsp.BranchAndCut
import ch.ethz.math.ifor.atsp.BranchAndCut.{BranchNode, upperBoundSolver}
import ch.ethz.math.ifor.atsp.{Input, Output, Site, Solver, Tour, arcWise}
import com.google.ortools.linearsolver.{MPConstraint, MPObjective, MPSolver, MPVariable}

object BranchAndCutSolver extends Solver {
  def solve(input: Input): Output = {

    // construct root node
    val numSites: Int = input.sites.length
    val initAssignmentArray: Array[Array[Option[Boolean]]] = Array.ofDim[Option[Boolean]](numSites, numSites)
    val initAssignmentMap: Map[Site, Map[Site, Option[Boolean]]] = input.sites.zip(initAssignmentArray).map { case (site, distRow) =>
      site -> input.sites.zip(distRow).toMap
    }.toMap
    val rootNode: BranchNode = new BranchNode(input, initAssignmentMap)
    rootNode.level = 0
    rootNode.isRootNode = true

    // Need a global cuts pool
    var globalCuts: List[(Map[MPVariable,Double],Double)] = List()

    var activeBranches: List[BranchNode] = List(rootNode) // start with root node
    var currentBestNode: Option[BranchNode] = None

    val initUpperBound = upperBoundSolver.computeUpperBound(rootNode)

    // TODO: Question, why in FT97, they didn't check if the solution is integer before applying separation algos?

    //solve LP associated
    // apply AP-pricing
    // remove some cuts, update core set
    // apply separation algorithm to find cuts tht cut off the current LP solution
    // 1. check all cuts in global pool
    // 2. if no cuts found, apply PR90 MINCUT algorithm for SEC separation
    // 3. if no cuts found, shrink, apply separation algorithms for comb, D_k and odd CAT
    // 4. if some cuts are found, add to the current LP and repeat, else if (...)
    while (activeBranches.nonEmpty) {

      val sortedNodes: List[BranchNode] = activeBranches.filter(_.lowerBound <= initUpperBound).sortBy(_.lowerBound)

      // println("num sortedNodes active", sortedNodes.length)

      var currentBranchNode = sortedNodes.head //consider node with smallest lower bound
      activeBranches = sortedNodes.reverse.init //remove considered node from active nodes

      // apply AP-pricing
      val solutionAfterPricing: Map[Site, Map[Site, Double]] = pricingScheme.updateColumns(currentBranchNode)

      currentBranchNode.lowerBoundSolve = solutionAfterPricing

      val newCuts: List[(Map[MPVariable,Double],Double)] = cuttingPlane.findCuts(currentBranchNode)

      if (newCuts.nonEmpty){
        // add cuts to current node and add to the branch list
        currentBranchNode.fromCutToConstraint(newCuts)
        globalCuts = globalCuts ++ newCuts
        activeBranches = activeBranches ++ List(currentBranchNode)

      } else {
        // check if current solution is integer
        if (currentBranchNode.isInteger){
          // if integer, update current best solution
          currentBestNode = Some(currentBranchNode)
          activeBranches = activeBranches.filter(_.lowerBound <= currentBestNode.get.lowerBound)
        } else {
          // otherwise, branch and add to the branch list
          val children: List[BranchNode]=branchingScheme.listChildren(currentBranchNode)
          for (child <- children) {
            if (currentBestNode.isEmpty) {
              println("add this children", child)
              activeBranches = activeBranches ++ List(child)
            } else if (child.lowerBound < currentBestNode.get.lowerBound) { //first check a naive lower bound for child node
              activeBranches = activeBranches ++ List(child) //add children/new branches
            }
          }
        }
      }

    }

    val tour = currentBestNode.get.findTour(currentBestNode.get.lowerBoundSolve)
    new Output(input, tour)

  }
}
