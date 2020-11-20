package ch.ethz.math.ifor.atsp.BranchAndBound

import ch.ethz.math.ifor.atsp.{Input, Tour}
import com.google.ortools.linearsolver.MPVariable

class LeafNode(input: Input, varAssignment: Map[MPVariable, Option[Boolean]]) extends Node(input, varAssignment) {

  //require that all variables are fixed
  assert(varAssignment.values.forall(_.isDefined), "not all variables are fixed")

    //TODO: might build LeafNodes differently to avoid potential re-computation
  val upperBound: Double = ???

  //TODO: might build LeafNodes differently to avoid potential re-computation
  val tour: Tour = ???
}
