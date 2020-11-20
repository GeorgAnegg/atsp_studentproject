package ch.ethz.math.ifor.atsp.BranchAndBound

import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.AssignmentProblem.ORToolsIP
import ch.ethz.math.ifor.atsp.Input
import com.google.ortools.linearsolver.MPVariable


/** abstract class for node classes like branch node and leaf node
  *
  * @param input
  * @param varAssignments contains information for which variables are already set to 0 or 1
  * @param level
  */
abstract class Node(input: Input, varAssignments: Map[MPVariable, Option[Boolean]]) {

  assert(lowerBoundSolvers.variables == varAssignments.keys, "var assignments don't match input")




}
