package ch.ethz.math.ifor

import Double.{NegativeInfinity, PositiveInfinity}

package object atsp {

  val inf: Double = PositiveInfinity
  val negInf: Double = NegativeInfinity
  type Runtime = Double
  def Runtime(x:Double):Runtime = x
}
