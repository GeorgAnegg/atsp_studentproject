package ch.ethz.math.ifor.atsp

class Output (val input: Input,
              val tour: Tour){

  /* require tour to include all sites */
  assert(tour.sequence.toSet == input.sites.toSet, "tour incomplete")


  def value:Double = tour.length

  def print(): Unit = {
    println("List of arcs on optimal solution:")
    tour.listArcsOrdered.foreach(e => println(e._1,e._2))
    println(s"""The optimal tour is
    ${tour.listArcsOrdered}
    which has length
    ${tour.length}""")
  }
}
