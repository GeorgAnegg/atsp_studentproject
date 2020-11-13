package ch.ethz.math.ifor.atsp

import com.google.ortools.linearsolver.MPVariable

class Input(val sites: Vector[Site],
            val distMat: Map[Site, Map[Site, Double]]) {

  //require distMat to be complete wrt sites
  //TODO: distance from point to itself should be 0 but can be ignored. require it to be present?
  assert(distMat.keys.toVector == sites && distMat.values.forall(_.keys.toVector == sites), "distance matrix incomplete")

  def distance(fromSite: Site, toSite: Site): Double = distMat(fromSite)(toSite)

  // TODO: figure out the best type to implement this
  val variables: Map[Site, Map[Site, MPVariable]] = ???

}


object Input {

  def fromDistVec(distVec: Vector[Vector[Double]]): Input = {

    val sites = Vector.fill(distVec.length)(new Site)

    val distMat = sites.zip(distVec).map{case (site, distRow) =>
    site -> sites.zip(distRow).toMap}.toMap

    new Input(sites, distMat)
  }

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