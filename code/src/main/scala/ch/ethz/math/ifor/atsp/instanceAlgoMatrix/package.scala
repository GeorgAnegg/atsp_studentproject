package ch.ethz.math.ifor.atsp

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchAndBoundSolver
import ch.ethz.math.ifor.atsp.BranchAndCut.BranchAndCutSolver
import ch.ethz.math.ifor.atsp.CompactFormulations.{DL, GG, MTZ2020}
import ch.ethz.math.ifor.atsp.dataProcessing.CSV
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix.timeOut.timed

package object instanceAlgoMatrix {

  val instances = List(
    "br17",
    "ftv33")/*,
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
  )*/

  val namedInputs:List[(String, Input)] = instances.map(name => (name , CSV.createInput(name+".csv")))

  val MTZoutput: Input=> Output = MTZ2020.solve(_)._2
  val GGoutput: Input=> Output = GG.solve(_)._2
  val DLoutput: Input=> Output = DL.solve(_)._2

  val namedSolvers :List[(String, Input => Output)] = List(
    ("CDT" , BranchAndBoundSolver.solve(_, "",true,false)),
    ("FT92" , BranchAndCutSolver.solve(_, "",true,true)),
    ("FT97", BranchAndCutSolver.solve(_, "",true,false)),
    ("MTZ_FT97", BranchAndCutSolver.solve(_,"MTZ",true,false)),
    ("MTZ", MTZoutput),
    ("GG", GGoutput),
    ("DL", DLoutput)
  )

<<<<<<< HEAD
  def runAll(maxTime: Int, input: Input): Map[String, Either[(Double, Runtime), String]] = namedSolvers.map {
    case (name, solver) => name -> timed(maxTime, input, solver)
  }.toMap
=======

  def runAll(maxTime: Int, input: String): List[(String, Either[(Double, Runtime), String])] = namedSolvers.map {
    case (name, solver) => (name , timed(maxTime, namedInputs.find( _._1 == input).get._2 , solver))
  }



  // matrix of values
  def instanceAlgoData(maxTime: Int): List[(String, List[(String, Either[(Double, Runtime), String])])] = instances.map(name => name ->
  runAll(maxTime, name))


>>>>>>> babd24e850b567f63130e00aa8cfb6e65f836eef
}
