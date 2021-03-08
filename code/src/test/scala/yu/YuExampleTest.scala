package yu

import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchAndBoundSolver, BranchNode, rSAPLowerBoundSolver}
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.Arborescence.ChuLiuEdmonds
import ch.ethz.math.ifor.atsp.dataProcessing
import ch.ethz.math.ifor.atsp.dataProcessing.Spreadsheet
import ch.ethz.math.ifor.atsp.{Input, Site}
import ch.ethz.math.ifor.atsp.Input.{fromDistVec, toyExample4, toyExample5}

object YuExampleTest extends App {
  // execute this with 'test:run' in the sbt shell
  print("This is a test \n")
  val a = Spreadsheet
  val t1 = System.nanoTime

  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/distance_matrix.csv",";")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/br17.csv",";")
  val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/little1963.csv",";")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/gr17.csv",";")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/p15.csv",";")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/fri26.csv"," ")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/dantzig42.csv"," ")
  //val input = a.createInput("/Users/yudeng/Desktop/atsp/raw_data/att48.csv"," ")

/*

  // test r-SAP

  val output = rSAPLowerBoundSolver.compute(fromDistVec(input))
  //val output = rSAPLowerBoundSolver.compute(toyExample5)
  println("==========================output====================================")
  output.collect{
    case(site1,map1) => (site1,map1.collect{
      case (site2, value) if value => println(site1.id,site2.id,output(site1)(site2))
    })
  }
  //val rSAPLB = output.map({case(site1, map1) => inputPrime.distMat(site1)(map1.filter(_._2).head._1) }).sum
  //println("Optimal length is: "+rSAPLB)

 */


  val output = BranchAndBoundSolver.solve(fromDistVec(input),"",true,false)

  output.print()
  val duration = (System.nanoTime - t1) / 1e9d
  println("Run time:" + duration)


}
