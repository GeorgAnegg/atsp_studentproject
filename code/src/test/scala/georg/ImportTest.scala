package georg


import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchAndBoundSolver, BranchNode, rSAPLowerBoundSolver}
import ch.ethz.math.ifor.atsp.BranchAndCut.{BranchAndCutSolver, BranchNode}
import ch.ethz.math.ifor.atsp.CompactFormulations.{MTZ2020,DL,GG}

import ch.ethz.math.ifor.atsp.dataProcessing.CSV

object ImportTest extends App {

  val instances = List(
    "br17",
    "ftv33",
    "ftv35",
    "ftv38",
    "p43",
    "ftv44",
    "ftv47",
    "ry48p",
    "ft53",
    "ftv55",
    "ftv64",
    "ftv70",
    "ft70",
    "kro124p",
    "ftv170",
    "rbg323",
    "rbg358",
    "rbg403",
    "rbg443"
  )

  val input = CSV.createInput("ftv35.csv")
  val t1 = System.nanoTime
  val output = BranchAndCutSolver.solve(input, "MTZ",true,true,true)
  //val output = MTZ2020.solve(input)
  val duration = (System.nanoTime - t1) / 1e9d
  output.print()

  println("Run time:" + duration)
}
