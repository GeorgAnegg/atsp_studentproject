package ch.ethz.math.ifor.atsp.BranchAndBound.branchingSchemes

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchNode
import ch.ethz.math.ifor.atsp.Site

import scala.util.control.Breaks.{break, breakable}

object CT80 extends BranchingScheme {

  def chooseSubtour(branchNode: BranchNode):List[BranchNode]={

    var result: List[BranchNode] = List()
    var excludedArcs: Map[Site,Site] = Map()
    var includedArcs: Map[Site,Site] = Map()

    for (map1 <- branchNode.sitesStatus){
      for (map2 <- map1._2){
        // continue if value is null
        breakable {
          if (map2._2 == null) break
        }
        if (map2._2.get) excludedArcs += map1._1 -> map2._1
        else includedArcs += map1._1 -> map2._1
      }
    }

    //choose the subtour with minimum number of arcs not included in includedArcs
    var bestSubtour: Map[Site,Site] = Map()
    var currentBest = 9999

    for (tour <- branchNode.allTours){
      val listArcs = tour.listArcs
      var count = 0
      for (arc <- listArcs){
        if (excludedArcs.exists(x => x._1 == arc._1 && x._2 == arc._2)) count += 1
      }
      if (listArcs.size - count < currentBest) {
        bestSubtour = listArcs
        currentBest = listArcs.size - count
      }
    }

    for (arc <- bestSubtour){
      val childMap = collection.mutable.Map[Site,Map[Site,Option[Boolean]]]() ++= branchNode.sitesStatus
      childMap(arc._1)(arc._2) = true //why...?




    }





    result
  }

}
