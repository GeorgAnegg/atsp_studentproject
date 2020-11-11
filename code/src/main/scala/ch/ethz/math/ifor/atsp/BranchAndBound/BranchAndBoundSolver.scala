package ch.ethz.math.ifor.atsp.BranchAndBound

import ch.ethz.math.ifor.atsp.{Input, Output, Solver}

object BranchAndBoundSolver extends Solver {


  // TODO: construct root node
  val rootNode:BranchNode = ???

  def solve(input: Input): Output = {

    var currentBestNode: Option[LeafNode] = None

    // start with root node
    var activeBranches: List[BranchNode] = List(rootNode)


    while (activeBranches.nonEmpty)  {

      val sortedNodes = activeBranches.sortBy(_.lowerBound)

      val currentBranchNode = sortedNodes.head //consider node with smallest lower bound
      activeBranches = sortedNodes.init //remove considered node from active nodes

      currentBranchNode.branchStep match {
        case Left(leaf) => // current node is leaf
          if (leaf.upperBound<currentBestNode.get.upperBound){ //compare with current upper bound
            currentBestNode=Some(leaf)
            activeBranches=activeBranches.filter(_.lowerBound>currentBestNode.get.upperBound)//prune remaining branches
          }
        case Right(children) => // current node gets branched
          activeBranches =  activeBranches ++ children //add children/new branches
      }
    }


    val tour = currentBestNode.get.tour
    new Output(input, tour)
  }

}
