package ch.ethz.math.ifor.atsp

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchAndBoundSolver
import ch.ethz.math.ifor.atsp.BranchAndCut.BranchAndCutSolver
import ch.ethz.math.ifor.atsp.CompactFormulations.{DL, GG, MTZ}
import ch.ethz.math.ifor.atsp.dataProcessing.CSV
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix.timeOut._

package object instanceAlgoMatrix {

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

  val namedInputs:List[(String, Input)] = instances.map(name => (name , CSV.createInput(name+".csv")))

  val namedSolvers :List[(String, Input => Output)] = List(
    ("CDT" , BranchAndBoundSolver.solve(_, "",true,false,true,true)),
    ("FT92" , BranchAndBoundSolver.solve(_, "",true,true,true,true)),
    ("FT97", BranchAndCutSolver.solve(_, "",true,false,false,false)),
    ("MTZ_FT97", BranchAndCutSolver.solve(_,"MTZ",true,false,false,false)),
    //("DL_FT97", BranchAndCutSolver.solve(_,"DL",true,false,false,false)),
    ("MTZ", MTZ.solve(_,"",true, false, false,false)),
    ("GG", GG.solve(_,"",true, false, false,false)),
    ("DL", DL.solve(_,"",true, false, false,false))
  )

  def runAll(maxTime: Int, input: String): List[(String, Either[(Int, Double, Double, Runtime), String])] = namedSolvers.map {
    case (name, solver) => {
        println(s"Solving instance $input with $name")
      val result = timed2(maxTime, namedInputs.find( _._1 == input).get._2 , solver)
      //System.gc()
      //Thread.sleep(1*1000)
      (name, result)
    }
  }

  // matrix of values
  def instanceAlgoData(maxTime: Int): List[(String, List[(String, Either[(Int, Double, Double, Runtime), String])])] = instances.map(name => name ->
  runAll(maxTime, name))

}
