package ch.ethz.math.ifor.atsp.BranchAndBound.branchingSchemes

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchNode
import ch.ethz.math.ifor.atsp.{Site, inf}

import scala.util.control.Breaks.{break, breakable}
import scala.collection.mutable

object CT80 extends BranchingScheme {

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

    /*
    branchNode.sitesStatus.map((site1,map2)=>(site1,map2 match{ case (site2, null)=>(site2, null)
                                                              case (site2, true)=> excludedArcs += site1 -> map2._1
                                                              case (site2, false)=> includedArcs += site1 -> map2._1
    }))
    */

    // choose the subtour with minimum number of arcs not included in includedArcs
    var bestSubtour: Map[Site,Site] = Map()
    var currentBest = inf

    for (tour <- branchNode.allTours){
      val listArcs = tour.listArcs
      var count = 0
      for (arc <- listArcs){
        if (includedArcs.exists(x => x._1 == arc._1 && x._2 == arc._2)) count += 1
      }
      if (listArcs.size - count < currentBest) {
        bestSubtour = listArcs
        currentBest = listArcs.size - count
      }
    }

    var listArcs = bestSubtour.toList

    // compute h_j
    var list_h:List[Int] = List.fill(listArcs.length)(0)
    var unwanted_index: List[Int] = List()
    for (arc <- includedArcs){
      val a = listArcs.indexOf(arc)
      if (a != -1 && a!=0 && a!= list_h.length-1) {
        list_h.patch(a-1, Seq(list_h(a-1) + 1), 1)
        list_h.patch(a+1, Seq(list_h(a+1) + 1), 1)
      } else if (a == 0) {
        list_h.patch(a+1, Seq(list_h(a+1) + 1), 1)
        list_h.patch(list_h.length-1, Seq(list_h.last+ 1), 1)
      } else if (a == list_h.length-1) {
        list_h.patch(a-1, Seq(list_h(a-1) + 1), 1)
        list_h.patch(0, Seq(list_h.head+ 1), 1)
      }
      unwanted_index = unwanted_index ::: List(a)
    }

    list_h = list_h.zipWithIndex.collect {
      case (x,i) if !unwanted_index.contains(i) => x
    }

    val unwanted = includedArcs.toSet
    listArcs = listArcs.filterNot(unwanted)

    // compute w_j
    var list_w: List[Int] = List()
    for (index <- listArcs.indices){
      var init = 0
      for(index1 <- index to listArcs.length){
        init += list_h(index1)*(listArcs.length-1-index1+index)
      }
      for(index2 <- 1 to index-2){
        init += list_h(index2)*(index-1-index2)
      }
      list_w = list_w ::: List(init)
    }

    // sort children
    var children_pair = listArcs zip list_w
    children_pair = children_pair.sortBy(_._2)
    listArcs = children_pair.map(_._1)


    // create children nodes
    for (i <- listArcs.indices){
      // create a varAssignment based on that of parent
      var childMap = mutable.Map[Site, Map[Site, Option[Boolean]]]()
      for (item <- branchNode.sitesStatus){
          childMap += item
        }
      // add the ith arc to excluded arc set
      childMap(listArcs(i)._1).updated(listArcs(i)._2, false)
      // add the 1st to (i-1)th arcs to included arc set
      for (j <- 0 until i){
        childMap(listArcs(j)._1).updated(listArcs(j)._2, true)
      }
      // return a new branchNode with new updated varAssignment, and add to the result list
      listChildrenNodes = new BranchNode(branchNode.inputNode, childMap.toMap) :: listChildrenNodes
    }
    listChildrenNodes
  }
}
