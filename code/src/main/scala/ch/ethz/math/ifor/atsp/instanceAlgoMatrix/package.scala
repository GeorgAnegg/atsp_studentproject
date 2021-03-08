package ch.ethz.math.ifor.atsp

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchAndBoundSolver
import ch.ethz.math.ifor.atsp.BranchAndCut.BranchAndCutSolver
import ch.ethz.math.ifor.atsp.CompactFormulations.{DL, GG, MTZ2020}
import ch.ethz.math.ifor.atsp.dataProcessing.CSV
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix.timeOut.timed

package object instanceAlgoMatrix {

  val instances = List(
    "br17",
    "ftv170",
    "ftv38",
    "ftv55",
    "kro124p",
    "rbg358",
    "ry48p",
    "ft53",
    "ftv33",
    "ftv44",
    "ftv64",
    "p43",
    "rbg403",
    "ft70",
    "ftv35",
    "ftv47",
    "ftv70",
    "rbg323",
    "rbg443"
  )
  
  val namedInputs:Map[String, Input] = instances.map(name => name -> CSV.createInput(name+".csv")).toMap


  val namedSolvers :Map[String, Input => Output] = List(
    ("CDT" , BranchAndBoundSolver.solve(_, "",true,true)),
    ("FT92" , BranchAndCutSolver.solve(_, "",true,true)),
    ("FT97", BranchAndCutSolver.solve(_, "",true,false)),
    ("MTZ_FT97", BranchAndCutSolver.solve(_,"MTZ",true,false))//,
    //("MTZ_MIP", MTZ2020.solve(_)),
    //("GG", GG.solve(_)),
    //("DL", DL.solve(_))
  ).toMap


  def runAll(maxTime: Int, input: Input): Map[String, Either[(Double, Runtime), String]] = namedSolvers.map {
    case (name, solver) => name -> timed(maxTime, input, solver)
  }.toMap



  // matrix of values
  def instanceAlgoData(maxTime: Int): Map[ String, Map[String, Either[(Double, Runtime), String]]] = instances.map(name => name ->
  runAll(maxTime, namedInputs(name))).toMap


}
