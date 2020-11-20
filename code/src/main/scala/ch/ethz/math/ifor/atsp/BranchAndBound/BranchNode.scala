package ch.ethz.math.ifor.atsp.BranchAndBound


import ch.ethz.math.ifor.atsp.Input
import com.google.ortools.linearsolver.MPVariable

class BranchNode(input: Input,
                 varAssignment: Map[MPVariable, Option[Boolean]]
                 )
  extends Node(input, varAssignment) {


  def lowerBoundSolve:(LowerBound, IsLeafNode) = lowerBoundSolver.compute(branchNode = this)

  val lowerBound: LowerBound  = lowerBoundSolve._1
  val isLeafNode: IsLeafNode = lowerBoundSolve._2


  // TODO: implement branchStep
  def branchStep: Either[LeafNode, List[BranchNode]] = {

    // either returns LeafNode at current BranchNode,
    // or returns children of current BranchNode


    //TODO: depending on what lowerBoundSolvers compute and output, adjust how LeafNodes are built
    if (isLeafNode) { //if lower bound is feasible tour, create leaf node
      val toLeafNode: LeafNode = ???
      Left(toLeafNode)
    }
    else { //else use branching rule to get subproblems
      Right(List()) //list of children
    }
  }

}
