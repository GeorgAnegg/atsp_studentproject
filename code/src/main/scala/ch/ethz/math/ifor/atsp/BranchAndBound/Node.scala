package ch.ethz.math.ifor.atsp.BranchAndBound

// TODO: replace "Double" by the type of a variable
abstract class Node(vars: Map[Double, Option[Int]]) {

  def lowerBound: Double = ??? //compute lower bound by solving AP

}
