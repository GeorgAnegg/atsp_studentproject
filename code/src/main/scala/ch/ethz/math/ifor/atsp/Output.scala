package ch.ethz.math.ifor.atsp

class Output (val input: Input,
              val tour: List[Site]){

  /* require tour to include all sites */
  assert(tour.toSet == input.sites, "tour incomplete")


  val tourLength: Double =
    (tour.zipWithIndex map {
      case (site, index) if site==tour.last => input.distance(site, tour(0))
      case (site, index) => input.distance(site, tour(index+1))
    }).sum


  def print(): Unit =
    println(s"""The optimal tour is
    $tour
    which has length
    $tourLength""")
}
