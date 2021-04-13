package ch.ethz.math.ifor

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchAndCutSolver

import Double.{NegativeInfinity, PositiveInfinity}

package object atsp {

  val inf: Double = PositiveInfinity
  val negInf: Double = NegativeInfinity
  type Runtime = Double
  def Runtime(x:Double):Runtime = x

  /** This solver takes as input a distance matrix as Vector[Vector[Double] (row by row) and returns a tuple (value, tour) containing the optimal value as well as the (indices of) the corresponding optimal tour
    * This Branch and Cut algorithm is based on Fischetti, Toth ('97)
    *
    *
     * @param distMat
    * @return
    */
  def solve(distMat: Vector[Vector[Double]]): (Double, List[Int]) = {
    val input= Input.fromDistVec(distMat)
    println("Running ATSP solver...")
    val output = BranchAndCutSolver.solve(input, "",true,false,false,false)

    val tour = output.tour.sequence.map(input.sites.indexOf(_))
    val value = output.value
    (value, tour)
  }
}
