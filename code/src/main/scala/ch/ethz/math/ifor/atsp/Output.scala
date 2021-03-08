package ch.ethz.math.ifor.atsp

class Output (val input: Input,
              val tour: Tour){

  /* require tour to include all sites */
  assert(tour.sequence.toSet == input.sites.toSet, "tour incomplete")


  def value:Double = tour.length

  def print(): Unit =
    println(s"""The optimal tour is
    ${tour.sequence}
    which has length
    ${tour.length}""")
}
