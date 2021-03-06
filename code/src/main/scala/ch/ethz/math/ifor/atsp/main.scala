package ch.ethz.math.ifor.atsp

import ch.ethz.math.ifor.atsp.dataProcessing.CSV

import scala.concurrent._
import scala.concurrent.duration._



object main extends App {

  val filenames= List("br17.csv",
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

  val timeout = 1.seconds.fromNow

  def evalFuture(solver: Input=> Output, input:Input, timeout:Deadline): Option[Output]= {
    val output = solver(input)
    Thread.sleep(4000)
    if (timeout.isOverdue) {return None}
    Some(output)
  }
  def deadlineWrapper(solver:Input=>Output, input:Input, timeout:Deadline):Option[Output]={
    import ExecutionContext.Implicits.global

    val fut = Future {evalFuture(solver, input, timeout)}
    val output = Await.result(fut,Duration.Inf)
    output
  }


  val input = CSV.createInput(filenames(0))

  val output = {
    //val timeout = 8.seconds.fromNow
    deadlineWrapper(BranchAndCut.BranchAndCutSolver.solve(_,""), input, timeout)
  }

  println(output)




  

 /* set up spreadsheet, check car rental

   def allAlgos(instanceName:String): Map[String, Output] = {
   val input = Input.fromFilename(instanceName)

   //Deadlinewrapper for solve!

   Map(
   algoName1->algo1.solve(input) ,
   ....
   )
   }

   val data = filenames.map(instanceName -> allAlgos(instanceName) )

   data.foreach{case (instanceName, allOutputs) =>

    Spreadsheet.writeLine(outputs)
*/
}
