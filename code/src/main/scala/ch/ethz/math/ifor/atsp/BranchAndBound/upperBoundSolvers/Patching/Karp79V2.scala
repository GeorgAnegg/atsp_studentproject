package ch.ethz.math.ifor.atsp.BranchAndBound.upperBoundSolvers.Patching
import ch.ethz.math.ifor.atsp.BranchAndBound.upperBoundSolvers.UpperBoundSolver

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchNode
import ch.ethz.math.ifor.atsp.{Site, Tour, inf}

object Karp79V2 extends UpperBoundSolver {
  def computeUpperBound(branchNode: BranchNode): (Double,Tour) = {

    var upperBound: Double = branchNode.lowerBoundCostAP

    branchNode.allTours.foreach(e => e.listArcsOrdered.foreach { e => println(e._1, e._2,branchNode.costsMap(e._1)(e._2)) })

    var tours: List[List[(Site, Site)]] = List()
    branchNode.allTours.foreach(tour => tours = tours :+ tour.listArcsOrdered)

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

      val startIndexV1 = currentShortestCycle.indexOf(arc1)
      var newTour : List[(Site, Site)] = List()
      newTour = newTour :+ (arc1._1, arc2._2)

      val patchedCycle = tours(cycles.last)
      val startIndex:Int = patchedCycle.indexOf(arc2)
      //println("startindex: ",startIndex,tours(cycles.last).size,startIndex-tours(cycles.last).size)
      for (i <- startIndex+1 until tours(cycles.last).size){
        //println(patchedCycle(i)._1,patchedCycle(i)._2)
          newTour = newTour :+ patchedCycle(i)
      }
      for (i <- 0 until startIndex){
          newTour = newTour :+ patchedCycle(i)
      }
      newTour = newTour :+ (arc2._1, arc1._2)

      for (i <- startIndexV1+1 until currentShortestCycle.size){
        //println(patchedCycle(i)._1,patchedCycle(i)._2)
        newTour = newTour :+ currentShortestCycle(i)
      }
      for (i <- 0 until startIndexV1){
        newTour = newTour :+ currentShortestCycle(i)
      }
      // update tours by deleting two such cycles and adding a new combined one
      tours = tours.patch(cycles.last, Nil, 1)
      tours = tours ::: List(newTour)

      // add two such arcs in listParched to avoid being patched again
      listPatched = listPatched ::: Map(arc1) :: Nil
      listPatched = listPatched ::: Map(arc2) :: Nil
      upperBound += minCostPermute
    }

    val finalCircle = tours.head
    var listTour : List[Site] =List()
    finalCircle.foreach{
      case (site1,site2) => listTour = listTour :+ site1
    }
    listTour.foreach{e => println(e)}
    val tour = new Tour(branchNode.input,listTour)
    (upperBound,tour)
  }

}
