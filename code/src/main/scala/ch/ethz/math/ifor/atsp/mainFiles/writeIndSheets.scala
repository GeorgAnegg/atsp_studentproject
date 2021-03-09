package ch.ethz.math.ifor.atsp.mainFiles

import java.io.{File, FileOutputStream}

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchAndBoundSolver
import ch.ethz.math.ifor.atsp.dataProcessing.CSV
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix.{instances, namedSolvers}
import ch.ethz.math.ifor.atsp.instanceAlgoMatrix.timeOut.timed
import org.apache.poi.xssf.usermodel.XSSFWorkbook

object writeIndSheets extends App {

  writeSheet("ftv64", 5)

  val instances = List(
    "br17",
    "ftv33",
    "ftv35",
    "ftv38",
    "p43",
    "ftv44",
    "ftv47",
    "ry48p",
    "ft53",
    "ftv55",
    "ftv64",
    "ftv70",
    "ft70",
    "kro124p",
    "ftv170",
    "rbg323",
    "rbg358",
    "rbg403",
    "rbg443"
  )

  def writeRows(workbook: XSSFWorkbook, input: String, maxTime: Int): Unit = {

    val data = instanceAlgoMatrix.runAll(maxTime, input)


    val sheet = workbook.createSheet(input)

    //write header row
    val headerRow = sheet.createRow(0)
    val columns = List(input) ::: namedSolvers.map(_._1)
    for (i <- columns.indices) {
      val cell = headerRow.createCell(i)
      cell.setCellValue(columns(i))
    }

    var rowCounter = 1
    //fill in data
    data.foreach { entry =>
      val row = sheet.createRow(rowCounter)
      val nameCell = row.createCell(0)
      nameCell.setCellValue("Running Time")

      for (i <- data.indices) {
        val cell = row.createCell(i + 1)
        cell.setCellValue(data(i)._2 match {
          case Left(pair) => pair._2.toString
          case Right(s) => s
        })
      }
    }

    rowCounter = 2
    //fill in data
    data.foreach { entry =>
      val row = sheet.createRow(rowCounter)
      val nameCell = row.createCell(0)
      nameCell.setCellValue("Opt Value")

      for (i <- data.indices) {
        val cell = row.createCell(i + 1)
        cell.setCellValue(data(i)._2 match {
          case Left(pair) => pair._1.toString
          case Right(s) => s
        })
      }
    }

  }


  def writeSheet(input: String, maxTime: Int) = {
    val filename = System.getProperty("user.dir") + s"/tests/$input.xlsx"
    val workbook = new XSSFWorkbook()

    writeRows(workbook, input, maxTime)

    val fileOut = new FileOutputStream(filename)
    workbook.write(fileOut)

    fileOut.close()
    workbook.close()

    println("====================")
    println(" XLSX file written to:")
    println(filename)
    println("====================")
  }


}




