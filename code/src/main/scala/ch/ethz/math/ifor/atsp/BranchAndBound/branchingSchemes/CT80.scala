package ch.ethz.math.ifor.atsp.BranchAndBound.branchingSchemes

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchNode
import ch.ethz.math.ifor.atsp.{Site, Tour, inf}

import scala.::
import scala.util.control.Breaks.{break, breakable}
import scala.collection.mutable

object CT80 extends BranchingScheme {

  def listChildren(branchNode: BranchNode):List[BranchNode]={

    var listChildrenNodes: List[BranchNode] = List()
    var excludedArcs: Map[Site,Site] = Map()
    var includedArcs: Map[Site,Site] = Map()

    println("parent node",branchNode.sitesStatus)
    for (i <- branchNode.sitesStatus){
      for (j<-i._2){
        print(j._2+"  ")
      }
      println("  ")
    }

//TODO: use iterableObject.map instead of for (thing <- iterableObject) when possible
    for (map1 <- branchNode.sitesStatus){
      for (map2 <- map1._2){
        if (map2._2 != null) {
          if (map2._2.get) includedArcs += map1._1 -> map2._1
          else excludedArcs += map1._1 -> map2._1
        }
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
    //println("best tour",bestSubtour)

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
      for(index1 <- index until listArcs.length){
        init += list_h(index1)*(listArcs.length-1-index1+index)
      }
      for(index2 <- 1 to index-2){
        init += list_h(index2)*(index-1-index2)
      }
      list_w = list_w ::: List(init)
    }
    // sort children
    var children_pair = listArcs zip list_w
    children_pair = children_pair.sortBy(_._2)(Ordering[Int].reverse)
    listArcs = children_pair.map(_._1)

    println("w value")
    for (i <- children_pair){
      println(i._1,i._2)
    }
    //println("list arcs",listArcs)

    // create children nodes
    for (i <- listArcs.indices){
      // create a varAssignment based on that of parent
      var childMap = mutable.Map[Site, Map[Site, Option[Boolean]]]()
      for (item <- branchNode.sitesStatus){
          childMap += item
        }
      //println(childMap)
      // add the ith arc to excluded arc set
      childMap = childMap.collect{case site1-> map1 => site1 -> map1.collect{
        case site2-> _ if site1.id==listArcs(i)._1.id && site2.id==listArcs(i)._2.id => site2 ->Some(false)
        case site2-> bool if site1.id!=listArcs(i)._1.id || site2.id!=listArcs(i)._2.id => site2 -> bool
      }}
      // add the 1st to (i-1)th arcs to included arc set
      for (j <- 0 until i){
        //println("now some true, j=, i=",j,i)
        childMap = childMap.collect{case site1-> map1 => site1 -> map1.collect{
          case site2-> _ if site1.id==listArcs(j)._1.id && site2.id==listArcs(j)._2.id => site2 ->Some(true)
          case site2-> bool if site1.id!=listArcs(j)._1.id || site2.id!=listArcs(j)._2.id => site2 -> bool
        }}
      }

      // TODO: exclude additional arcs according to Little. 1963

      def avoidPotentialCycle(lbSolve:mutable.Map[Site, Map[Site, Option[Boolean]]]): Unit = {

        // pairMap contains pair of sites in Included Arc Set

        println("do we have there",excludedArcs.size, includedArcs.size)
        for (i <- lbSolve){
          for (j<-i._2){
            print(j._2+"  ")
          }
          println("  ")
        }

        var pairMap = scala.collection.mutable.Map[Site, Site]()
        for (i <- lbSolve){
          for (j<-i._2){
            if (j._2 != null){
              if (j._2.contains(true)){
                pairMap =  pairMap + (i._1->j._1)
              }
            }
          }
        }
        println("pair map size before",pairMap.size)

        //var pairMap = lbSolve.map({ case (site1, map1) => site1 -> map1.filter(_._2 != null).head._1 })
        println("hello",pairMap.size)
        //pairMap = lbSolve.map({ case (site1, map1) => site1 -> map1.filter(_._2.contains(true)).head._1 })
        println("hello",pairMap.size)

        var listTours: List[List[Site]] = List()
        var currentList :List[Site] = List(pairMap.head._1, pairMap.head._2)

        var currentArc = pairMap.head


        while (pairMap.size>1) {

          pairMap = pairMap.-(currentArc._1)

          while (pairMap.exists(_._2.id == currentArc._1.id)) {
            val previousArc = pairMap.find(_._2.id == currentArc._1.id).get
            currentList = previousArc._2::currentList
            currentList = previousArc._1::currentList
            pairMap = pairMap.-(previousArc._1)
            currentArc = previousArc
          }

          currentArc = Map(currentList.lift(currentList.length-2).get->currentList.lastOption.get).head

          while (pairMap.exists(_._1.id == currentArc._2.id)) {
            val nextArc = pairMap.find(_._1.id == currentArc._2.id).get
            currentList = currentList:::nextArc._1::Nil
            currentList = currentList:::nextArc._2::Nil
            pairMap = pairMap.-(nextArc._1)
            currentArc = nextArc
          }

          listTours = listTours:::currentList::Nil
          currentList = currentList.drop(currentList.length)

          if (pairMap.nonEmpty) {
            currentArc = pairMap.head
            currentList = currentList ::: currentArc._1 :: Nil
            currentList = currentList ::: currentArc._2 :: Nil
          }
        }

        // exclude all arcs that could create a cycle if included
        for (item <- listTours){
          if (item.length > 2){
            val fromSite = item.lastOption.get
            val toSite = item.head
            childMap = childMap.collect{case site1-> map1 => site1 -> map1.collect{
              case site2-> _ if site1.id==fromSite.id && site2.id==toSite.id => site2 ->Some(false)
              case site2-> bool if site1.id!=listArcs(i)._1.id || site2.id!=listArcs(i)._2.id => site2 -> bool
            }}
          }
        }
      }

      if (includedArcs.nonEmpty){avoidPotentialCycle(childMap)}

      println("childmap",childMap)
      // return a new branchNode with new updated varAssignment, and add to the result list
      var newNode = new BranchNode(branchNode.inputNode, childMap.toMap)
      // link children to parent, update level
      newNode.parentNode = branchNode
      newNode.level = branchNode.level + 1
      listChildrenNodes = newNode :: listChildrenNodes
    }
    // link children to parent, update level
    listChildrenNodes
  }
}
