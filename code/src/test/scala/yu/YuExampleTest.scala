package yu

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchAndBoundSolver
import ch.ethz.math.ifor.atsp.dataProcessing
import ch.ethz.math.ifor.atsp.dataProcessing.Spreadsheet
import ch.ethz.math.ifor.atsp.Input
import ch.ethz.math.ifor.atsp.Input.fromDistVec

object YuExampleTest extends App {
  // execute this with 'test:run' in the sbt shell
  print("This is a test \n")
  val a = Spreadsheet
  val t1 = System.nanoTime

  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/distance_matrix.csv")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/br17.csv")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/little1963.csv")
  val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/gr17.csv")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/p15.csv")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/fri26.csv")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/dantzig42.csv")

  val output = BranchAndBoundSolver.solve(fromDistVec(input))

  output.print()
  val duration = (System.nanoTime - t1) / 1e9d
  println("Run time:" + duration)


}
