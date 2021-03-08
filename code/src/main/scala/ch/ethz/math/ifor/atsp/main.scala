package ch.ethz.math.ifor.atsp

import java.io.FileOutputStream

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchAndBoundSolver
import ch.ethz.math.ifor.atsp.dataProcessing.CSV
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix.namedSolvers
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix.timeOut.timed
import org.apache.poi.xssf.usermodel.XSSFWorkbook



object main extends App {

  val data = instanceAlgoMatrix.instanceAlgoData(3)

  writeSheet()



  def writeRunningTimes(workbook: XSSFWorkbook)= {


    val sheet = workbook.createSheet("runningTimes")
    val headerRow = sheet.createRow(0)
    val columns = List(" ") ::: namedSolvers.keys.toList


  }


  def writeOptValues(workbook: XSSFWorkbook)= ???

  def writeSheet()={
    val filename = System.getProperty("user.dir") + "/tests/allAlgos.xlsx"
    val workbook = new XSSFWorkbook()

    writeRunningTimes(workbook)
    writeOptValues(workbook)

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



