package ch.ethz.math.ifor.atsp.BranchAndBound.upperBoundSolvers.Patching

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchNode
import ch.ethz.math.ifor.atsp.BranchAndBound.upperBoundSolvers.UpperBoundSolver
import ch.ethz.math.ifor.atsp.{Output, Site, Tour, inf}

object Karp79 extends UpperBoundSolver {

  def computeUpperBound(branchNode: BranchNode): (Double,Tour) = {

    var upperBound: Double = branchNode.lowerBoundCostAP

    var tours: List[Map[Site, Site]] = List()
    branchNode.allTours.foreach(tour => tours = tours ::: tour.listArcs :: Nil)

    def costPermute(arc1: (Site, Site), arc2: (Site, Site)): Double = {
      branchNode.costsMap(arc1._1)(arc2._2) + branchNode.costsMap(arc2._1)(arc1._2) -
        branchNode.costsMap(arc1._1)(arc1._2) - branchNode.costsMap(arc2._1)(arc2._2)
    }

    var listPatched: List[Map[Site, Site]] = List()

    while (tours.size > 1) {

      tours = tours.sortBy(_.size)

      var minCostPermute = inf

      val currentShortestCycle = tours.head

      tours = tours.drop(1)

      var arcPairs: List[(Site, Site)] = List()
      var cycles: List[Int] = List()

      for (arc1 <- currentShortestCycle) {
        for (cycle <- tours) {
          for (arc2 <- cycle) {
            val costPermute12 = costPermute(arc1, arc2)
            if (costPermute12 < minCostPermute && !listPatched.contains(arc1) && !listPatched.contains(arc2)) {
              minCostPermute = costPermute12
              arcPairs = arcPairs ::: arc1 :: Nil
              arcPairs = arcPairs ::: arc2 :: Nil
              cycles = cycles ::: tours.indexOf(cycle) :: Nil
            }
          }
        }
      }
      val arc1 = arcPairs(arcPairs.size - 2)
      val arc2 = arcPairs.last

      // construct new tour by combine two cycles and delete/add two arcs
      var newTour: Map[Site, Site] = currentShortestCycle.++(tours(cycles.last))
      newTour = newTour.updated(arc1._1, arc2._2)
      newTour = newTour.updated(arc2._1, arc1._2)

      // update tours by deleting two such cycles and adding a new combined one
      tours = tours.patch(cycles.last, Nil, 1)
      tours = tours ::: List(newTour)

      // add two such arcs in listParched to avoid being patched again
      listPatched = listPatched ::: Map(arc1) :: Nil
      listPatched = listPatched ::: Map(arc2) :: Nil
      upperBound += minCostPermute
    }

    val finalCircle = tours.head
    val listTour : List[Site] = finalCircle.keys.toList
    val tour = new Tour(branchNode.input,listTour)
    (upperBound,tour)
  }


}
