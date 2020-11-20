package yu

import ch.ethz.math.ifor.atsp.BranchAndBound.AssignmentProblem.AssignmentProblemSolver

object AssignmentProblemTest extends App {


  //val input = Input.toyExample
  val costs: Vector[Vector[Double]] =
    Vector(
      Vector(100, 1, 10, 10),
      Vector(10, 100, 1, 10),
      Vector(10, 10, 100, 1),
      Vector(1, 10, 10, 100)
    )

  AssignmentProblemSolver.sol(costs)

}
