package georg

import ch.ethz.math.ifor.atsp.Input

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchAndBoundSolver

object BranchAndBoundTest extends App {

  val input = Input.toyExample3

  val output = BranchAndBoundSolver.solve(input)

  output.print()

}
