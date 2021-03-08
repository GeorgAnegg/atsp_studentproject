package georg


import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchAndBoundSolver, BranchNode, rSAPLowerBoundSolver}
import ch.ethz.math.ifor.atsp.BranchAndCut.{BranchAndCutSolver, BranchNode}

import ch.ethz.math.ifor.atsp.dataProcessing.CSV

object ImportTest extends App {
  val filenames= List("br17.csv",
    "ftv170.csv",
    "ftv38.csv",
    "ftv55.csv",
    "kro124p.csv",
    "rbg358.csv",
    "ry48p.csv",
    "ft53.csv",
    "ftv33.csv",
    "ftv44.csv",
    "ftv64.csv",
    "p43.csv",
    "rbg403.csv",
    "ft70.csv",
    "ftv35.csv",
    "ftv47.csv",
    "ftv70.csv",
    "rbg323.csv",
    "rbg443.csv")

  val input = CSV.createInput("br17.csv")
  val t1 = System.nanoTime
  val output = BranchAndBoundSolver.solve(input, "",true,true)
  val duration = (System.nanoTime - t1) / 1e9d
  //output.print()

  println("Run time:" + duration)


}
