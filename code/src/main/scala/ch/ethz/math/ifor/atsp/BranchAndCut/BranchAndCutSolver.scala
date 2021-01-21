package ch.ethz.math.ifor.atsp.BranchAndCut
import ch.ethz.math.ifor.atsp.BranchAndCut.{BranchNode, upperBoundSolver}
import ch.ethz.math.ifor.atsp.{Input, Output, Site, Solver, Tour}
import optimus.optimization.model.MPConstraint

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

    // initial constraints


    def updateNode(node: BranchNode):BranchNode={
      //solve LP associated
      val currentLPSolution: Map[Site, Map[Site, Double]] = linearProgrammingSolver.findSolution(rootNode)

      // apply AP-pricing
      val solutionAfterPricing: Map[Site, Map[Site, Double]] = pricingScheme.updateColumns(rootNode)

      // update core set

      node
    }

    // apply separation procedure to find cuts
    // if found, add to current LP
    // if not, and if x integer, then update current optimal solution
    // if not, and if x fractional, then branch
    var activeBranches: List[BranchNode] = List(rootNode) // start with root node
    var currentBestNode: Option[BranchNode] = None

    val initUpperBound = upperBoundSolver.computeUpperBound(rootNode)


    while (activeBranches.nonEmpty) {

      val sortedNodes: List[BranchNode] = activeBranches.filter(_.lowerBound <= initUpperBound).sortBy(_.lowerBound)

      // println("num sortedNodes active", sortedNodes.length)

      var currentBranchNode = sortedNodes.head //consider node with smallest lower bound
      activeBranches = sortedNodes.reverse.init //remove considered node from active nodes
      /*
            println("active branches after sorted")
            for (i <- activeBranches) {
              println(i, i.lowerBound, i.level)
            }
            println("\r\n")
            println("current branchnode", currentBranchNode.level, "parent", currentBranchNode.parentNode.level)

       */

      var newCuts: List[MPConstraint] = cuttingPlane.findCuts(currentBranchNode)
      while (newCuts.nonEmpty){
        currentBranchNode = updateNode(currentBranchNode)
        newCuts = cuttingPlane.findCuts(currentBranchNode)
      }

      if (currentBranchNode.isInteger){
        currentBestNode = Some(currentBranchNode)
        activeBranches = activeBranches.filter(_.lowerBound <= currentBestNode.get.lowerBound)
      } else {
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

    val tour = new Tour(input,input.sites.toList) // TODO
    new Output(input, tour)

  }
}
