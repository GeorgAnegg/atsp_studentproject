package ch.ethz.math.ifor.atsp.BranchAndBound

import ch.ethz.math.ifor.atsp.Site

// TODO: replace "Double" by the type of a variable
class LeafNode(vars: Map[Double, Option[Int]]) extends Node(vars) {

  //require that all variables are fixed
  assert(vars.values.forall(_.isDefined), "not all variables are fixed")

  //TODO: compute upper bound
  val upperBound: Double = ???

  // TODO: get tour from leaf Node
  val tour: List[Site] = ???
}
