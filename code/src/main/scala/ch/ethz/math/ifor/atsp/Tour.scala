package ch.ethz.math.ifor.atsp

class Tour (val input: Input,
            val sequence: List[Site]) {

  val length: Double =
    (sequence.zipWithIndex map {
      case (site, _) if site == sequence.last => input.distance(site, sequence.head)
      case (site, index) => input.distance(site, sequence(index + 1))
    }).sum

  val listArcs: Map[Site, Site] =
    (sequence.zipWithIndex map {
      case (site, _) if site == sequence.last => site -> sequence.head
      case (site, index) => site -> sequence(index + 1)
    }).toMap

}