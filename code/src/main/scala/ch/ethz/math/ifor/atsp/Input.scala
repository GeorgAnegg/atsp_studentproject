package ch.ethz.math.ifor.atsp

import com.google.ortools.linearsolver.MPVariable
import ch.ethz.math.ifor.atsp.{Site, Tour}

class Input(val sites: Vector[Site],
            val distMat: Map[Site, Map[Site, Double]]) {

  //require distMat to be complete wrt sites
  //TODO: distance from point to itself should be 0 but can be ignored. require it to be present?
  assert(distMat.keys.toVector == sites && distMat.values.forall(_.keys.toVector == sites), "distance matrix incomplete")

  def distance(fromSite: Site, toSite: Site): Double = distMat(fromSite)(toSite)

}


object Input {

  def fromDistVec(distVec: Vector[Vector[Double]]): Input = {

    val sites = Vector.fill(distVec.length)(new Site)

    val distMat = sites.zip(distVec).map{case (site, distRow) =>
    site -> sites.zip(distRow).toMap}.toMap

    new Input(sites, distMat)
  }

  def computeReducedDistVec(distVec: Vector[Vector[Double]]): (Vector[Vector[Double]], Double) = {
    var reducedCost: Double = 0

    for (i <- distVec.indices){
      // set ii entry to a large number to avoid loops
      distVec(i).updated(i, inf)

      //reduce each column
      val minCol = distVec.map{_(i)}.min
      distVec.foreach(vec => vec.foreach{case (vec(i)==dist) dist => dist-minCol})
      reducedCost += minCol

      //reduce each row
      val minRow = distVec(i).min
      distVec(i).foreach(dist => dist - minRow)
      reducedCost += minRow
    }

    (distVec, reducedCost)

  }

  //TODO:
  def reducedCostExcluded(distVec: Vector[Vector[Double]], arcsExcluded: Map[Site,Site])

  //TODO:
  def reducedCostIncluded(distVec: Vector[Vector[Double]], arcsExcluded: Map[Site,Site])


  val toyExample: Input = {

    val toyDistVec: Vector[Vector[Double]] =
      Vector(
        Vector(0.0, 1, 10, 10),
        Vector(10, 0, 1, 10),
        Vector(10, 10, 0, 1),
        Vector(1, 10, 10, 0)
      )

    fromDistVec(toyDistVec)
  }

}