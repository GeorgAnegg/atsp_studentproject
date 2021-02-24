package yu

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchAndCutSolver
import ch.ethz.math.ifor.atsp.Input.fromDistVec
import ch.ethz.math.ifor.atsp.dataProcessing.Spreadsheet

object BranchAndCutTest extends App{

  print("This is a test \n")
  val a = Spreadsheet
  val t1 = System.nanoTime

  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/distance_matrix.csv",";")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/br17.csv",";")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/little1963.csv",";")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/gr17.csv",";")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/p15.csv",";")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/fri26.csv"," ")
  val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/dantzig42.csv"," ")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/att48.csv"," ")

  val output = BranchAndCutSolver.solve(fromDistVec(input),"MTZ")

  output.print()
  val duration = (System.nanoTime - t1) / 1e9d
  println("Run time:" + duration)

}
