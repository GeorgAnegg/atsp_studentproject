package ch.ethz.math.ifor.atsp.BranchAndBound.connectingProcedure
import ch.ethz.math.ifor.atsp.{Site, Tour, inf}
import ch.ethz.math.ifor.atsp.BranchAndBound.BranchNode
object ConnectingProcedure {

  def reduceNumberOfSubtours(branchNode: BranchNode): Map[Site,Map[Site,Boolean]] = {

    var modifiedSolution = branchNode.lowerBoundSolve.to(collection.mutable.Map)

    val zeroArcsThreshold:Double = branchNode.input.sites.length * 2.5

    val numberOfZeros:Double={
      var count = 0
      branchNode.reducedCostMatrix.foreach{
        case (_,map1) => map1.foreach{ case (_,value) if value == 0 => count += 1}
      }
      count
    }

    if(numberOfZeros > zeroArcsThreshold) {

      var tours: List[Map[Site, Site]] = List()
      branchNode.allTours.foreach(tour => tours = tours ::: tour.listArcs :: Nil)

      def filterZeroArcs(arcs: Map[Site, Site]): Map[Site, Site] = {
        arcs.collect { case (site1, site2) if branchNode.reducedCostMatrix(site1)(site2) == 0.0 => (site1, site2) }
      }

      var zeroTours = tours.zipWithIndex.collect {
        case (tour, index) if filterZeroArcs(tour).nonEmpty => (filterZeroArcs(tour), index)
      }

      while (zeroTours.size >= 2) {

        val currentTour1 = zeroTours.head._1
        tours = tours.drop(zeroTours.head._2)

        zeroTours = zeroTours.drop(1)

        val currentTour2 = zeroTours.head._1
        tours = tours.drop(zeroTours.head._2)

        zeroTours = zeroTours.drop(1)

        val arc1 = currentTour1.head
        val arc2 = currentTour2.head

        var newTour = currentTour1.++(currentTour2)
        newTour = newTour.updated(arc1._1, arc2._2)
        newTour = newTour.updated(arc2._1, arc1._2)

        tours = tours ::: newTour :: Nil

        modifiedSolution = modifiedSolution.map {
          case (site1, map1) => (site1, map1.map {
            case (site2, _) if site1 == arc1._1 && site2 == arc1._2 => (site2, false)
            case (site2, _) if site1 == arc2._1 && site2 == arc2._2 => (site2, false)
            case (site2, _) if site1 == arc1._1 && site2 == arc2._2 => (site2, true)
            case (site2, _) if site1 == arc2._1 && site2 == arc1._2 => (site2, true)
            case (site2, value) if !(site1 == arc1._1 && site2 == arc1._2) && !(site1 == arc2._1 && site2 == arc2._2)
              && !(site1 == arc1._1 && site2 == arc2._2) && !(site1 == arc2._1 && site2 == arc1._2) => (site2, value)
          })
        }

        zeroTours = tours.zipWithIndex.collect {
          case (tour, index) if filterZeroArcs(tour).nonEmpty => (filterZeroArcs(tour), index)
        }
      }
    }
    modifiedSolution.toMap
    }















}
