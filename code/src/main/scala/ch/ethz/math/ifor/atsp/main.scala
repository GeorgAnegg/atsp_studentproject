package ch.ethz.math.ifor.atsp

import java.io.FileOutputStream

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchAndBoundSolver
import ch.ethz.math.ifor.atsp.dataProcessing.CSV
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix.{namedSolvers,instances}
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix.timeOut.timed
import org.apache.poi.xssf.usermodel.XSSFWorkbook



object main extends App {

  val data = instanceAlgoMatrix.instanceAlgoData(1)

  writeSheet()



  def writeRunningTimes(workbook: XSSFWorkbook)= {


    val sheet = workbook.createSheet("runningTimes")

    //write header row
    val headerRow = sheet.createRow(0)
    val columns = List(" ") ::: namedSolvers.map(_._1)
    for (i<- columns.indices) {
      val cell = headerRow.createCell(i)
      cell .setCellValue(columns(i))
    }

    var rowCounter = 1
    //fill in data
    data.foreach {
      case (name, entries) => {
        val row = sheet.createRow(rowCounter)
        rowCounter+=1
        val nameCell = row.createCell(0)
        nameCell.setCellValue(name)

        for (i<- entries.indices){
          val cell = row.createCell(i+1)
          cell.setCellValue(entries(i)._2 match {
            case Left(pair)=> pair._2.toString
            case Right(s) => s
          } )
        }
      }

    }


  }


  def writeOptValues(workbook: XSSFWorkbook)= ???

  def writeSheet()={
    val filename = System.getProperty("user.dir") + "/tests/allAlgos.xlsx"
    val workbook = new XSSFWorkbook()

    writeRunningTimes(workbook)
    //writeOptValues(workbook)

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



