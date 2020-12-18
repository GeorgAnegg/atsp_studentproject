package ch.ethz.math.ifor.atsp.BranchAndBound.upperBoundSolver
import ch.ethz.math.ifor.atsp.{Site, inf}

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchNode

object Karp79 {

  def computeUpperBound(branchNode: BranchNode): Double = {

    var upperBound:Double = branchNode.lowerBound

    var tours: List[Map[Site,Site]] = List()
    branchNode.allTours.foreach(tour => tours = tours ::: tour.listArcs:: Nil)

    def costPermute(arc1:(Site, Site), arc2:(Site, Site)):Double = {
      branchNode.costsMap(arc1._1)(arc2._2) + branchNode.costsMap(arc2._1)(arc1._2) -
        branchNode.costsMap(arc1._1)(arc1._2) - branchNode.costsMap(arc2._1)(arc2._2)
    }

    var listPatched: List[Map[Site,Site]] = List()

    while (tours.size>1){

      tours = tours.sortBy(_.size)

      var minCostPermute = inf

      val currentShortestCycle = tours.head

      tours = tours.drop(1)

      for (arc1 <- currentShortestCycle){
        for (cycle <- tours){
          for (arc2 <-cycle){
            val costPermute12 = costPermute(arc1,arc2)
            if (costPermute12 < minCostPermute && !listPatched.contains(arc1) && !listPatched.contains(arc2)) {
              minCostPermute = costPermute12
              listPatched = listPatched ::: Map(arc1) :: Nil
              listPatched = listPatched ::: Map(arc2) :: Nil
            }
          }
        }
      }
      upperBound += minCostPermute
    }
    upperBound
  }




}
