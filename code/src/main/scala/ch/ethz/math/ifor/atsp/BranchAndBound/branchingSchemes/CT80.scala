package ch.ethz.math.ifor.atsp.BranchAndBound.branchingSchemes

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchNode
import ch.ethz.math.ifor.atsp.Site

import scala.util.control.Breaks.{break, breakable}

object CT80 extends BranchingScheme {
//TODO: implement choosing 'first' arc in subtour by computing weights w_j
  def listChildren(branchNode: BranchNode):List[BranchNode]={

    var listChildrenNodes: List[BranchNode] = List()
    var excludedArcs: Map[Site,Site] = Map()
    var includedArcs: Map[Site,Site] = Map()
//TODO: use iterableObject.map instead of for (thing <- iterableObject) when possible
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
    // TODO: use inf (defined in atsp.package)
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

    val listArcs = bestSubtour.toList
    for (i <- listArcs.indices){
      val childMap = collection.mutable.Map[Site,Map[Site,Option[Boolean]]]() ++= branchNode.sitesStatus
      // add the jth arc to excluded arc set
      childMap(listArcs(i)._1)(listArcs(i)._2) = false //why...?
      // add the 1st to (j-1)th arcs to included arc set
      for (j <- 0 until i){
        childMap(listArcs(j)._1)(listArcs(j)._2) = true
      }
      // return a new branchnode with new updated varAssignment, and add to the result list
      listChildrenNodes = new BranchNode(branchNode.inputNode, childMap) :: listChildrenNodes
    }
    listChildrenNodes
  }
}
