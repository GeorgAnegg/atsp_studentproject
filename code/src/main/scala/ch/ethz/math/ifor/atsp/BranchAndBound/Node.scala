package ch.ethz.math.ifor.atsp.BranchAndBound

import ch.ethz.math.ifor.atsp.Input
import com.google.ortools.linearsolver.MPVariable
import ch.ethz.math.ifor.atsp.BranchAndBound.AssignmentProblem.AssignmentProblemSolver


//input.variables
abstract class Node(input: Input, varAssignments: Map[MPVariable, Option[Boolean]], level: Int) {

  assert(input.variables == varAssignments.keys, "var assignments don't match input")

  def lowerBound: Double = ??? //compute lower bound by solving AP
  //set up AP problem

  //add constraints to AP problem

  //have exactly one arc going out of each node
  input.sites.map( site =>
  input.variables(site).values
  )

  //have exactly one arc coming into each node
  //TODO: scala exercise


}
