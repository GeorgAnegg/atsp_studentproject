package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.Arborescence

import ch.ethz.math.ifor.atsp.BranchAndBound.BranchNode
import ch.ethz.math.ifor.atsp.{Input, Site, Tour, arcWise}
import com.google.ortools.linearsolver.MPVariable

object ChuLiuEdmonds {

  def compute(branchNode:BranchNode):Map[Site, Map[Site, Boolean]]={

    // choose an arbitrary root node
    val rootSite = branchNode.input.sites(0)
    // construct cost graph
    val costs:arcWise[Double] = arcWise(branchNode.input,branchNode.input.distance)

    // implement ChuLiuEdmonds algorithm to find the shortest spanning arborescence rooted at vertex r
    def chuLiuEdmonds(graph:Map[Site, Map[Site, Double]]): Map[Site, Map[Site, Boolean]] = {

      // for each node except root node, choose the min-cost in-degree site
      var minCost:Map[Site,Double]=Map()
      var minInDegreeMap:Map[Site,Site]=Map()

      branchNode.input.sites.foreach(site => minInDegreeMap= minInDegreeMap.++(inDegreeSites(graph, site)._1))
      branchNode.input.sites.foreach(site => minCost= minCost.++(inDegreeSites(graph, site)._2))

      // if minInDegreeMap does not contain a cycle, then it's the shortest spanning arborescence rooted at vertex r
      val firstCycleFound = detectCycles(minInDegreeMap,branchNode.input).head.sequence
      if (firstCycleFound.isEmpty){

        def constructArborescence(site1:Site ,site2:Site):Boolean=
          if (minInDegreeMap.contains(site1)){
            if(minInDegreeMap(site1)==site2){
              true
            }
            else {false}
          } else {false}

        val result: arcWise[Boolean] = arcWise(branchNode.input, constructArborescence)
        return result.entries
      }

      // if minInDegreeMap contains a cycle

      // construct the reduced cost graph
      val reducedCosts:Map[Site, Map[Site, Double]] = reducedCostMap(graph,minCost)

      // construct the shrinked graph, by contracting cycle to a single supernode
      val shrinkedGraph:Map[Site, Map[Site, Double]] = shrinkGraph(reducedCosts,firstCycleFound,minCost)

      // call ChuLiuEdmonds recursively
      val treePrime = chuLiuEdmonds(shrinkedGraph)

      // TODO: Extend treePrime to an arborescence by adding all but one edge of the cycle

      treePrime

    }

    // return the minimum cost in degree site and the corresponding cost
    def inDegreeSites(graph:Map[Site, Map[Site, Double]],site:Site):(Map[Site,Site],Map[Site,Double])={
      var result: List[(Site,Double)] = List()
      graph.foreach{
        case (site1, map1) => (site1,map1.foreach{
          case (site2, value) if site2 == site && site1 != site2 && site1 != rootSite
          => result = result ::: (site1,value) :: Nil
        }
        )
      }
      val sorted_list = result.sortBy(_._2)
      val minInSite = sorted_list.head._1
      val minInCost = sorted_list.head._2
      (Map(minInSite->site),Map(site->minInCost))
    }

    // subtract each in degree by min cost
    def subtractInDegree(graph:Map[Site, Map[Site, Double]],site:Site,cost:Double):Map[Site, Map[Site, Double]]={
      graph.collect{
        case (site1, map1) => (site1,map1.collect{
          case (site2, value) if site2 == site => (site2, value-cost)
          case (site2, value) if site2 != site => (site2, value)
        })
      }
    }

    def reducedCostMap(graph:Map[Site, Map[Site, Double]],minCost:Map[Site,Double]):Map[Site, Map[Site, Double]]={
      var result = graph.to(collection.mutable.Map)
      branchNode.input.sites.foreach(site => result = subtractInDegree(result.toMap, site, minCost(site)).to(collection.mutable.Map))
      result.toMap
    }

    def detectCycles(pairMap:Map[Site, Site],input: Input):List[Tour] = {

      var pairMaps = pairMap.to(collection.mutable.Map)

      var listTours: List[Tour] = List()
      var currentList :List[Site] = List(pairMaps.head._1, pairMaps.head._2)

      var currentArc = pairMaps.head

      while (pairMaps.size>1) {

        var nextArc = pairMaps.find(_._1.id == currentArc._2.id).get
        // if no tours created, keep tracking
        if (nextArc._2.id != currentList.head.id) {
          // currentList  = currentList:::nextArc._1::Nil
          currentList  = currentList:::nextArc._2::Nil
          pairMaps = pairMaps-currentArc._1
          currentArc  = nextArc
        } else {
          // else, add the tour created, and staring tracking another remaining arc
          // currentList  = currentList:::nextArc._1::Nil
          // currentList  = currentList:::nextArc._2::Nil
          val findTour = new Tour(input,currentList)
          listTours = listTours:::findTour::Nil
          currentList = currentList.drop(currentList.length)
          pairMaps = pairMaps-currentArc._1
          pairMaps = pairMaps-nextArc._1
          if (pairMaps.nonEmpty) {
            currentArc = pairMaps.head
            currentList = currentList ::: currentArc._1 :: Nil
            currentList = currentList ::: currentArc._2 :: Nil
          }
        }
      }
      listTours
    }

    def shrinkGraph(graph:Map[Site, Map[Site, Double]],cycle:List[Site],minCost:Map[Site,Double]):Map[Site, Map[Site, Double]]={
      val newSite = new Site()
      val newMap = graph.collect{
        case (site1,map1) => (site1,map1.collect{
          case(site2, value) if !cycle.contains(site1) && cycle.contains(site2) => (newSite, value-minCost(site2))
          case(site2, value) if !cycle.contains(site1) && !cycle.contains(site2) => (site2, value)
          case(site2, value) if cycle.contains(site1) && !cycle.contains(site2) => (site2, value)
        })
      }
      val newMap2 = newMap.collect{
        case (site1, map1) if cycle.contains(site1) => (newSite,map1)
      }
      newMap2
    }

    chuLiuEdmonds(costs.entries)

  }








}
