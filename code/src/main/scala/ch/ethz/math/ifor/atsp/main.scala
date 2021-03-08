package ch.ethz.math.ifor.atsp

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchAndBoundSolver
import ch.ethz.math.ifor.atsp.dataProcessing.CSV
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix.namedSolvers
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix.timeOut.timed



object main extends App {


  def runAll(maxTime: Int, input: Input): Map[String, Either[(Double, Runtime), String]] = namedSolvers.map {
    case (name, solver) => name -> timed(maxTime, input, solver)
  }.toMap



  

  //THIS IS FOR TESTING

  val maxTime = 2
  val input = CSV.createInput("rbg443.csv")
  val solver: Input => Output = BranchAndBoundSolver.solve(_, "", true, true)

  val timedResult = timed(maxTime, input, solver)


  println(timedResult match {
    case Left(out) => s"objective value ${out._1}, running time ${out._2}"
    case Right(s) => s
  }
  )

}



