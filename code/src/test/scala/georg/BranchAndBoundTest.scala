package georg

import ch.ethz.math.ifor.atsp.Input

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchAndBoundSolver

object BranchAndBoundTest extends App {

  val input = Input.toyExample2

  val output = BranchAndBoundSolver.solve(input)

  output.print()

}
