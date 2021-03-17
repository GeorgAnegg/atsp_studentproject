import java.io.{File, FileInputStream, FileOutputStream}

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchAndBoundSolver
import ch.ethz.math.ifor.atsp.dataProcessing.CSV
import ch.ethz.math.ifor.atsp.{Runtime, instanceAlgoMatrix}
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix.{instances, namedSolvers}
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix.runAll
import org.apache.poi.ss.usermodel.{Workbook, WorkbookFactory}
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import org.apache.poi.xssf.usermodel.XSSFSheet
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import java.io.FileInputStream

object instAlgoIncremental
{
  def main(args: Array[String]): Unit = {//this takes solverID InstanceName as input

    val rowNumber = 1+instances.indexOf(args(0).dropRight(4))
    val colNumber = 1+namedSolvers.map(_._1).indexOf(args(1))

    val either =
    try {
      val start = System.nanoTime
      val dur = Runtime((System.nanoTime - start) / 1e9d)
      println(s"solving ${args(0).dropRight(4)} with solver ${args(1)}")
      val output = namedSolvers.find(_._1 == args(1)).get._2(CSV.createInput(args(0)))
      Left(output.value, dur)
    }
    catch {
      case e: Any => {println(e)
        Right("ERROR")}
    }



    val file = new FileInputStream(new File("instAlgoMatrix_60.xlsx"))

    val workbook = new XSSFWorkbook(file)


    val filename = "/u/ganegg/Documents/atsp/code/instAlgoMatrix_60.xlsx" //System.getProperty("user.dir") + s"/src/main/resources/allAlgosIncr.xlsx"
    //val inp = getClass.getResourceAsStream("/allAlgosIncr.xlsx") // PROBLEMATIC
    //val workbook = WorkbookFactory.create(new File(filename))

    writeCell(rowNumber,colNumber,either,workbook)

    val fileOut = new FileOutputStream(filename)
    workbook.write(fileOut)

    fileOut.close()
    workbook.close()

  }

def writeCell (rowNumber:Int,
               colNumber:Int,
               either: Either[(Double, Runtime), String],
               workbook: Workbook): Unit = {

  List("optValues", "runningTimes").foreach(category => {
    val sheet = workbook.getSheet(category)
    var row = sheet.getRow(rowNumber)
    if (row ==null) {row = sheet.createRow(rowNumber) }


    println(s"writing value ${either} in row ${rowNumber} and column ${colNumber}")
    var cell = row.getCell(colNumber)
    if (cell == null) {
      cell = row.createCell(colNumber)}
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
  )

}


}
