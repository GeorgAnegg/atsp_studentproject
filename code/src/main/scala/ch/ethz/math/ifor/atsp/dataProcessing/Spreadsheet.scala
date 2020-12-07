package ch.ethz.math.ifor.atsp.dataProcessing

import java.io.{FileNotFoundException, IOException}

import ch.ethz.math.ifor.atsp.Input

object Spreadsheet {

  def createInput(filePath: String): Vector[Vector[Double]] = {
    // open csv file
    val bufferedSource = io.Source.fromFile(filePath)
    var distMatrix: Array[String] = Array()
    var c = 0
    //val iterableLine = bufferedSource.getLines
    var numSites = 0
    println(numSites)
    try {
      for (line <- bufferedSource.getLines) {
          val rows = line.split(";").map(_.trim)
          //val rows_value = rows.tail
          distMatrix = distMatrix ++ rows
          numSites += 1
          println(distMatrix.length, numSites)
      }
    } catch {
      case e: FileNotFoundException => println("Couldn't find that file.")
      case e: IOException => println("Got an IOException.")
    }

    var result_matrix: Array[Vector[Double]] = Array.ofDim[Vector[Double]](numSites)
    for (value <- 0 until numSites){
      val row = distMatrix.take(numSites)
      val result_row = row.map(x => x.toDouble).toVector
      result_matrix(value) = result_row
      distMatrix = distMatrix.drop(numSites)
    }
    bufferedSource.close
    println(result_matrix.length,result_matrix(0).length)
    for (i<-result_matrix){
      println(i)
    }
    result_matrix.toVector
  }
}

