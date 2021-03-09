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

  val listArcsOrdered: List[(Site, Site)] =
    (sequence.zipWithIndex map {
      case (site, _) if site == sequence.last => (site , sequence.head)
      case (site, index) => (site , sequence(index + 1))
    })
  /*
  val isValid : Boolean ={
    var res = true
    for (i <- listArcsOrdered.indices){
      if (listArcsOrdered(i)._2 != listArcsOrdered(i+1)._1){
        println("Error, arcs not consistent")
        res = false
      }
    }
    val listLeft:List[Site] = listArcsOrdered.map{
      case (site1,_) => site1
    }
    val listRight:List[Site] = listArcsOrdered.map{
      case (_,site2) => site2
    }

    if (listLeft.diff(input.sites).nonEmpty || input.sites.diff(listLeft).nonEmpty
    || listRight.diff(input.sites).nonEmpty || input.sites.diff(listRight).nonEmpty){
      println("Error, arcs not complete")
      res = false
    }
    res
  }

   */
}