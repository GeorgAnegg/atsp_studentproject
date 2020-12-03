package ch.ethz.math.ifor.atsp.BranchAndBound

import ch.ethz.math.ifor.atsp.{Input, Output, Solver,Site}

object BranchAndBoundSolver extends Solver {

  def solve(input: Input): Output = {

    // construct root node
    val numSites: Int = input.sites.length
    val initAssignmentArray: Array[Array[Option[Boolean]]] = Array.ofDim[Option[Boolean]](numSites, numSites)
    val initAssignmentMap: Map[Site, Map[Site, Option[Boolean]]] = input.sites.zip(initAssignmentArray).map{case (site, distRow) =>
      site -> input.sites.zip(distRow).toMap}.toMap
    val rootNode: BranchNode = new BranchNode(input, initAssignmentMap)

    var currentBestNode: Option[BranchNode] = None

    var activeBranches: List[BranchNode] = List(rootNode) // start with root node

    while (activeBranches.nonEmpty) {

      /** CT80 uses lowest-lower-bound search instead of depth-first search  */
      val sortedNodes: List[BranchNode] = activeBranches.sortBy(_.lowerBound)

      val currentBranchNode = sortedNodes.head //consider node with smallest lower bound
      activeBranches = sortedNodes.reverse.init //remove considered node from active nodes

      currentBranchNode.branchStep match {
        case Left(leaf) => // current node is leaf
          if (currentBestNode.isEmpty){
            currentBestNode = Some(leaf)
            activeBranches = activeBranches.filter(_.lowerBound > currentBestNode.get.lowerBound)
          } else if (leaf.lowerBound < currentBestNode.get.lowerBound) { //compare with current upper bound
            currentBestNode = Some(leaf)
            activeBranches = activeBranches.filter(_.lowerBound > currentBestNode.get.lowerBound) //prune remaining branches
          }
        case Right(children) => // current node gets branched
          for (child <- children){
            if (child.naiveLowerBound < currentBestNode.get.lowerBound){ //first check a naive lower bound for child node
              activeBranches = activeBranches ++ children //add children/new branches
            }
          }
      }
    }

    val tour = currentBestNode.get.allTours.head
    new Output(input, tour)
  }
}
