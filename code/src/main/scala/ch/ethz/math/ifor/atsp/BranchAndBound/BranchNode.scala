package ch.ethz.math.ifor.atsp.BranchAndBound

// TODO: replace "Double" by the type of a variable
class BranchNode(vars:Map[Double, Option[Int]]) extends Node(vars) {



  // TODO: implement branchStep
  def branchStep: Either[LeafNode,List[BranchNode]] = {

    // either returns LeafNode at current BranchNode,
    // or returns children of current BranchNode

    val isLeafNode: Boolean = ??? //check if lower bound solution is feasible tour

    if (isLeafNode) { //if lower bound is feasible tour, create leaf node
      val toLeafNode: LeafNode = ???
      Left(toLeafNode)
    }
    else { //else use branching rule to get subproblems
      Right(List())//list of children
    }
  }



}
