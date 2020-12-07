package ch.ethz.math.ifor.atsp

import com.google.ortools.linearsolver.MPVariable
import com.google.ortools.linearsolver.MPConstraint;
import com.google.ortools.linearsolver.MPObjective;
import com.google.ortools.linearsolver.MPSolver;
import ch.ethz.math.ifor.atsp.{Site, Tour}

class Input(val sites: Vector[Site],
            val distMat: Map[Site, Map[Site, Double]]) {

  //require distMat to be complete wrt sites
  //TODO: distance from point to itself should be 0 but can be ignored. require it to be present?
  println(distMat.size, sites.length)
  //assert(distMat.keys.toVector == sites && distMat.values.forall(_.keys.toVector == sites), "distance matrix incomplete")

  def distance(fromSite: Site, toSite: Site): Double = distMat(fromSite)(toSite)

}


object Input {

  def fromDistVec(distVec: Vector[Vector[Double]]): Input = {

    val sites = Vector.fill(distVec.length)(new Site)

    val distMat = sites.zip(distVec).map{case (site, distRow) =>
    site -> sites.zip(distRow).toMap}.toMap

    println(distMat)

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

  val toyExample2: Input = {
    val toyDistVec: Vector[Vector[Double]] =
    Vector(
      Vector(0.0, 10, 15, 20),
      Vector(10, 0, 35, 25),
      Vector(15, 35, 0, 30),
      Vector(20, 25, 30, 0)
    )
    fromDistVec(toyDistVec)
  }

}