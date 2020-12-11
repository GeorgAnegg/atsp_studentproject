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

  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/distance_matrix.csv")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/br17.csv")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/little1963.csv")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/gr17.csv")
  val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/p15.csv")

  val output = BranchAndBoundSolver.solve(fromDistVec(input))

  output.print()

}
