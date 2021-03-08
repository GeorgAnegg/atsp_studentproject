package ch.ethz.math.ifor.atsp

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchAndBoundSolver
import ch.ethz.math.ifor.atsp.BranchAndCut.BranchAndCutSolver
import ch.ethz.math.ifor.atsp.CompactFormulations.{DL, GG, MTZ2020}
import ch.ethz.math.ifor.atsp.dataProcessing.CSV
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix.timeOut.timed

package object instanceAlgoMatrix {

  val filenames= List(
    "br17.csv",
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

  val namedInputs:List[(String, Input)] = {
    val namedInputs = filenames.map(filename => (filename, CSV.createInput(filename)))
    println("All Inputs created")
    namedInputs
  }

  val namedSolvers :List[(String, Input => Output)] = List(
    ("CDT" , BranchAndBoundSolver.solve(_, "",true,true)),
    ("FT92" , BranchAndCutSolver.solve(_, "",true,true)),
    ("FT97", BranchAndCutSolver.solve(_, "",true,false)),
    ("MTZ_FT97", BranchAndCutSolver.solve(_,"MTZ",true,false))//,
    //("MTZ_MIP", MTZ2020.solve(_)),
    //("GG", GG.solve(_)),
    //("DL", DL.solve(_))
  )


  def runAll(maxTime: Int, input: Input): Map[String, Either[(Double, Runtime), String]] = namedSolvers.map {
    case (name, solver) => name -> timed(maxTime, input, solver)
  }.toMap



}
