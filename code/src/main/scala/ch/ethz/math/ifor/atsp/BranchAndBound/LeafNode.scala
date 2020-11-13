package ch.ethz.math.ifor.atsp.BranchAndBound

import ch.ethz.math.ifor.atsp.{Input, Tour}
import com.google.ortools.linearsolver.MPVariable

class LeafNode(input: Input, varAssignment: Map[MPVariable, Option[Boolean]], level: Int) extends Node(input, varAssignment, level) {

  //require that all variables are fixed
  assert(varAssignment.values.forall(_.isDefined), "not all variables are fixed")

  //TODO: compute upper bound
  val upperBound: Double = ???

  // TODO: get tour from leaf Node
  val tour: Tour = ???
}
