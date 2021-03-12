package ch.ethz.math.ifor.atsp.mainFiles

import java.io.{File, FileInputStream, FileOutputStream}

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchAndBoundSolver
import ch.ethz.math.ifor.atsp.dataProcessing.CSV
import ch.ethz.math.ifor.atsp.{Runtime, instanceAlgoMatrix}
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix.{instances, namedSolvers}
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix.runAll
import org.apache.poi.ss.usermodel.{Workbook, WorkbookFactory}
import org.apache.poi.xssf.usermodel.XSSFWorkbook

object incremental
{
  def main(args: Array[String]): Unit = {
    val data = runAll(20, args(0).dropRight(4))
    writeToFile(args(0).dropRight(4), data)
    System.exit(0) //THIS SHUTS DOWN THE JVM TO CANCEL ONGOING FUTURES
  }





  def writeRows(workbook: Workbook, input:String, data:List[(String, Either[(Double, Runtime), String])])= {

    val rowCounter = instances.indexOf(input) + 1


    List("optValues", "runningTimes").foreach(category => {
      val sheet = workbook.getSheet(category)
      val row = sheet.createRow(rowCounter)
      val nameCell = row.createCell(0)
      nameCell.setCellValue(input)

      //fill in data
      data.zipWithIndex.foreach {
        case ((name, either), index) => {
          val column = index + 1
          println(s"writing value ${either} for ${name} in cell ${column}")
          var cell = row.getCell(column)
          if (cell == null) {
            cell = row.createCell(column)
            cell.setCellValue(either match {
              case Left(pair) => {
                category match {
                  case "optValues" => pair._1.toString
                  case "runningTimes" => pair._2.toString
                }
              }
              case Right(s) => s
            })
          }
        }
      }
    }
    )
  }



  def writeToFile(input:String, data: List[(String, Either[(Double, Runtime), String])])={
    val filename = System.getProperty("user.dir") + s"/src/main/resources/allAlgosIncr.xlsx"
    val inp = getClass.getResourceAsStream("/allAlgosIncr.xlsx")
    val workbook = WorkbookFactory.create(inp)
    writeRows(workbook, input, data)

    val fileOut = new FileOutputStream(filename)
    workbook.write(fileOut)

    fileOut.close()
    workbook.close()

    println("====================")
    println(" XLSX file written to:")
    println(filename)
    println("====================")
  }



  //THIS IS FOR TESTING
  /*
  val maxTime = 2
  val input = CSV.createInput("rbg443.csv")
  val solver: Input => Output = BranchAndBoundSolver.solve(_, "", true, true)

  val timedResult = timed(maxTime, input, solver)


  println(timedResult match {
    case Left(out) => s"objective value ${out._1}, running time ${out._2}"
    case Right(s) => s
  }
  )
*/
}



