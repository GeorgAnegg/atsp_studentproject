package georg

import ch.ethz.math.ifor.atsp.Input

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchAndBoundSolver

object GeorgExampleTest extends App {

  val input = Input.toyExample

  val output= BranchAndBoundSolver.solve(input)

  output.print()

}
