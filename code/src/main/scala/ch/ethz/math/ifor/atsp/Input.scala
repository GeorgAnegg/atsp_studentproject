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

    // println(distMat)

    new Input(sites, distMat)
  }

  def twoNodesTSP(distVec: Vector[Vector[Double]]):Input = {

    val sites = Vector.fill(2*distVec.length)(new Site)

    val distVecPrime = distVec.zipWithIndex.map {
      case (vec, index1) => vec.zipWithIndex.map {
        case (value, index2) if index1 == index2 => -inf
      }
    }

    val infMatrix: Vector[Vector[Double]] =Vector.fill(distVec.length, distVec.length)( inf )

    val upperMatrix: Vector[Vector[Double]] = (infMatrix, distVecPrime.transpose).zipped.map(_ ++ _)
    val lowerMatrix: Vector[Vector[Double]] = (distVecPrime,infMatrix).zipped.map(_ ++ _)

    val finalMatrix: Vector[Vector[Double]] = upperMatrix ++ lowerMatrix

    val distMat = sites.zip(finalMatrix).map{case (site, distRow) =>
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

  val toyExample3: Input = {
    val toyDistVec: Vector[Vector[Double]] =
      Vector(
        Vector(15.0, 7, 3),
        Vector(5, 6, 2),
        Vector(9, 4, 1)
      )
    fromDistVec(toyDistVec)
  }
  // test r-SAP
  val toyExample4: Input = {
    val toyDistVec: Vector[Vector[Double]] =
      Vector(
        Vector(0.0, -10, -4, -9),
        Vector(-8, 0, -10, -3),
        Vector(-2, -12, 0, -7),
        Vector(-11, -2, -6, 0)
      )
    fromDistVec(toyDistVec)
  }

  // test r-SAP
  val toyExample5: Input = {
    val toyDistVec: Vector[Vector[Double]] =
      Vector(
        Vector(inf,2.0,inf,7,inf,inf),
        Vector(inf,inf,6,inf,5,inf),
        Vector(inf,inf,inf,inf,inf,4),
        Vector(inf,1,inf,inf,inf,inf),
        Vector(inf,inf,inf,3,inf,2),
        Vector(inf,2,inf,inf,inf,inf)
      )
    fromDistVec(toyDistVec)
  }

}