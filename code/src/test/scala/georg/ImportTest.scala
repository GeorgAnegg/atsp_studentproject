package georg


import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchAndBoundSolver, BranchNode, rSAPLowerBoundSolver}
import ch.ethz.math.ifor.atsp.BranchAndCut.{BranchAndCutSolver, BranchNode}
import ch.ethz.math.ifor.atsp.CompactFormulations.{MTZ2020,DL,GG}

import ch.ethz.math.ifor.atsp.dataProcessing.CSV

object ImportTest extends App {

  val input = CSV.createInput("ftv35.csv")
  val t1 = System.nanoTime
  val output = BranchAndBoundSolver.solve(input, "",true,false,false)
  //val output = GG.solve(input)
  val duration = (System.nanoTime - t1) / 1e9d
  output.print()

  println("Run time:" + duration)
}
