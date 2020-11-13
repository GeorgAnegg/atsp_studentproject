package ch.ethz.math.ifor.atsp.BranchAndBound

import ch.ethz.math.ifor.atsp.Input
import com.google.ortools.linearsolver.MPVariable

class BranchNode(input: Input,
                 varAssignment: Map[MPVariable, Option[Boolean]],
                 level: Int)
  extends Node(input, varAssignment, level) {

  // TODO: implement branchStep
  def branchStep: Either[LeafNode, List[BranchNode]] = {

    // either returns LeafNode at current BranchNode,
    // or returns children of current BranchNode

    val isLeafNode: Boolean = ??? //check if lower bound solution is feasible tour

    if (isLeafNode) { //if lower bound is feasible tour, create leaf node
      val toLeafNode: LeafNode = ???
      Left(toLeafNode)
    }
    else { //else use branching rule to get subproblems
      Right(List()) //list of children
    }
  }

}
