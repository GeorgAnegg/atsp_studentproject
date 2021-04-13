package georg

import ch.ethz.math.ifor.atsp
import ch.ethz.math.ifor.atsp.Input

object tourOrder extends App {

  val distMat:Vector[Vector[Double]] = Vector(
    Vector(0.0, 10, 1, 10),
    Vector(10, 0, 10, 1),
    Vector(10, 1, 0, 10),
    Vector(1, 10, 10, 0)
  )

  val input = Input.fromDistVec(distMat)

  val (value, tour) = atsp.solve(distMat)
  println(value, tour)


  println(input.sites, input.distMat)


}
