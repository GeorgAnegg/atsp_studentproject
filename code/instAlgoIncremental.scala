import java.io.{File, FileInputStream, FileOutputStream}

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchAndBoundSolver
import ch.ethz.math.ifor.atsp.dataProcessing.CSV
import ch.ethz.math.ifor.atsp.{Runtime, instanceAlgoMatrix}
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix.{instances, namedSolvers}
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix.runAll
import org.apache.poi.ss.usermodel.{CellType, Workbook, WorkbookFactory}
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
      val input = CSV.createInput(args(0))
      val start = System.nanoTime
      
      println(s"solving ${args(0).dropRight(4)} with solver ${args(1)}")

      val output = namedSolvers.find(_._1 == args(1)).get._2(input)
      val dur = Runtime((System.nanoTime - start) / 1e9d)
      Left(output.value, dur)

    }
    catch {
      case e: Any => {println(e)
        Right("ERROR")}
    }

    val filename = s"instAlgoMatrix_60.xlsx"
    val file = new FileInputStream(new File(filename))

    val workbook = new XSSFWorkbook(file)
    val sheetNames:List[String] = List("optValues", "runningTimes")
//
//    sheetNames.foreach { name =>
//      val sheet = workbook.createSheet(name)
//      //create header row
//      val header = sheet.createRow(0)
//      namedSolvers.map(_._1).zipWithIndex.foreach {
//        case (solverID, index)=>
//          val cell = header.createCell(index)
//          cell.setCellValue(solverID)}
//      instances.zipWithIndex.foreach {
//        case (instanceID, index) =>
//          val row = sheet.createRow(index + 1)
//          val cell = row.createCell(0)
//          cell.setCellValue(instanceID)
//      }
//    }

    writeCell(rowNumber,colNumber,either,workbook, sheetNames)

    val filename_global = System.getProperty("user.dir")+s"/${filename}"

    val fileOut = new FileOutputStream(filename_global)
    workbook.write(fileOut)

    fileOut.close()

    workbook.close()

    System.exit(0)
  }

def writeCell (rowNumber:Int,
               colNumber:Int,
               either: Either[(Double, Runtime), String],
               workbook: Workbook, sheetNames: List[String]): Unit = {

  println(s"writing value ${either} in row ${rowNumber} and column ${colNumber}")

  sheetNames.foreach(category => {
    val sheet = workbook.getSheet(category)
    var row = sheet.getRow(rowNumber)
    if (row ==null) {row = sheet.createRow(rowNumber) }


    var cell = row.getCell(colNumber)
    if (cell == null) {
      cell = row.createCell(colNumber)
    cell.setCellValue("empty")}
    if (List("ERROR", "emtpy").contains(cell.getStringCellValue)) {
      cell.setCellValue(either match {
      case Left(pair) => {
        category match {
          case "optValues" => pair._1.toString
          case "runningTimes" => pair._2.toString
        }
      }
      case Right(s) => s
    })}

  }
  )

}

}
