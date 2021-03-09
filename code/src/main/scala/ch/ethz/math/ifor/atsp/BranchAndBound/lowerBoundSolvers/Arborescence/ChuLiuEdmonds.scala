package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.Arborescence

import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, LowerBound}
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.LowerBoundSolver
import ch.ethz.math.ifor.atsp.{Input, Site, Tour, arcWise, inf}

object ChuLiuEdmonds {

  def compute(input: Input):Double={

    var infeasible = true
    input.distMat.collect{
      case (site, value) => (site,value.collect{
        case (site, d) if d != inf => infeasible = false
      })
    }

    if (infeasible){
      return inf
    }

    // choose an arbitrary root node
    val rootSite = input.sites(0)
    // println("rootnode is: " + rootSite.id)
    // construct cost graph
    val costs:arcWise[Double] = arcWise(input,input.distance)
    // block ii entries
    var costsPrime = costs.entries.map{
      case (site1,map1) => (site1,map1.map{
        case (site2, _) if site1==site2 => (site2,inf)
        case (site2, value) if site1!=site2 => (site2,value)
      })
    }
    /*
    println("ChuLiuEdmonds print reduced cost matrix: ")
    costsPrime.foreach{
      case (site1, map1) => (site1, map1.map{
        case (site2, value) => println(site1, site2, value)
      })
    }

     */
    // root node should have no in-edges
    costsPrime = costsPrime.map{
      case (site1,map1) => (site1,map1-rootSite)
    }

    // implement ChuLiuEdmonds algorithm to find the shortest spanning arborescence rooted at vertex r
    def chuLiuEdmonds(graph:Map[Site, Map[Site, Double]]): Map[Site, Map[Site, Boolean]] = {

      /*
      println("=============================================graph=========================================")
      for(site1<-graph.keys){
        for (site2 <-graph(site1)){
          println(site1.id,site2._1.id,site2._2)
        }
      }
      println("==============================================================================================")

       */

      // for each node except root node, choose the min-cost in-site
      var minCost:Map[Site,Double]=Map()
      var minInDegreeMap:List[(Site,Site)]=List()

      for(site <- graph.keys){
        if (site != rootSite){
          //println("here"+site.id)
          minInDegreeMap= minInDegreeMap.++(inDegreeSites(graph, site)._1)
          minCost= minCost.++(inDegreeSites(graph, site)._2)
          //println("size",minInDegreeMap.size)
        }
      }
/*
      graph.keys.foreach{case site if site!= rootSite => minInDegreeMap= minInDegreeMap.++(inDegreeSites(graph, site)._1)}
      graph.keys.foreach{case site if site!= rootSite => minCost= minCost.++(inDegreeSites(graph, site)._2)}

 */
/*
      println("minInDegreeMap")
      minInDegreeMap.foreach{map => println(map._1.id,map._2.id,graph(map._1)(map._2))}

 */
      val inputHere = new Input(graph.keys.toVector,graph)
      // if minInDegreeMap does not contain a cycle, then it's the shortest spanning arborescence rooted at vertex r
      val firstCycleFound = detectCycles(minInDegreeMap,inputHere)

      if (firstCycleFound.isEmpty){
        def constructArborescence(site1:Site ,site2:Site):Boolean=
          if (minInDegreeMap.contains((site1,site2))){
            true
          } else {false}


        val result: arcWise[Boolean] = arcWise(inputHere, constructArborescence)

/*
        println("==========================no cycles found, return a tree here====================================")
        result.entries.collect{
          case(site1,map1) => (site1,map1.collect{
            case (site2, value) if value => println(site1.id,site2.id,graph(site1)(site2))
          })
        }
        println("==============================================================")

 */




        return result.entries
      }

      // if minInDegreeMap contains a cycle
      /*
      println("first cycle")
      firstCycleFound.head.sequence.foreach{site => println(site.id)}

       */

      // construct the reduced cost graph
      //val reducedCosts:Map[Site, Map[Site, Double]] = reducedCostMap(graph,minCost)

      val reducedCosts:Map[Site, Map[Site, Double]]  = graph.map{
        case (site1,map1) => (site1,map1.map{
          case (site2, value) if firstCycleFound.head.sequence.contains(site2) => (site2, value-minCost(site2))
          case (site2, value) if !firstCycleFound.head.sequence.contains(site2) => (site2, value)
        })
      }
/*
      println("reducedCosts")
      for(site1<-reducedCosts.keys){
        for (site2 <-reducedCosts(site1)){
          println(site1.id,site2._1.id,site2._2)
        }
      }

 */

      // construct the shrinked graph, by contracting cycle to a single supernode
      val shrinkingResult = shrinkGraph(reducedCosts,firstCycleFound.head.sequence,minCost)
      val shrinkingGraph :Map[Site, Map[Site, Double]] = shrinkingResult._1
      val minToCycle:Map[Site, Map[Site, Double]] = shrinkingResult._2
      val minFromCycle:Map[Site, Map[Site, Double]] = shrinkingResult._3
      val supernode:Site = shrinkingResult._4

      // call ChuLiuEdmonds recursively, treePrime is a tree on vertex set V/C + supernode
      val treePrime = chuLiuEdmonds(shrinkingGraph)
      var treeArcs : List[(Site,Site)]=List()
      var arcsInCycle:Map[Site,Site]= firstCycleFound.head.listArcs

      val inputPrime = new Input(reducedCosts.keys.toVector,reducedCosts)
/*
      println("tree prime returned here")
      treePrime.foreach{
        case (site1, map1) => map1.collect{
          case (site2, value) if value => println(site1.id,site2.id)
        }
      }

 */
      /*
      println("minFromCycle here")
      minFromCycle.foreach{
        case (site1, map1) => map1.collect{
          case (site2, value) => println(site1.id,site2.id,value)
        }
      }

       */

      treePrime.foreach{
        case (site1, map1) => map1.collect{
          case (site2, value) if value => treeArcs = treeArcs ::: (site1,site2) :: Nil
        }
      }

      // extend treePrime to an arborescence by adding all but one edge of the cycle C

      // if in treePrime, supernode has an out-edge supernode->u, replace it with v->u, where v is a min-cost node in the supernode
      var outSites:List[Site]= List()
      treeArcs = treeArcs.collect{
        case (site1,site2) if site1==supernode => (minFromCycle(site2).head._1,site2)
        case (site1,site2) if site1!=supernode => (site1,site2)
      }
/*
      println("treeArcs before")
      treeArcs.foreach(map => println(map._1.id, map._2.id))

      println("arcsInCycle before")
      arcsInCycle.foreach(map => println(map._1.id, map._2.id))

 */

      // in treePrime supernode has an in-edge u->supernode, replace it with u->v, where v is a min-cost node in the supernode
      var inSite = new Site()
      //println("inSite",inSite.id,"supernode",supernode.id)
      treeArcs.collect{
        case (site1,site2) if site2==supernode => inSite = site1
      }
      //println("inSite",inSite.id)
      val inEdgeInCycle = minToCycle(inSite).head._1
      treeArcs = treeArcs.filter(_!=(inSite,supernode))
      treeArcs = treeArcs ::: (inSite,inEdgeInCycle) :: Nil


      // then delete v' that v'->v in the cycle
      arcsInCycle = arcsInCycle - arcsInCycle.find(_._2==inEdgeInCycle).get._1

/*
      println("treeArcs after")
      treeArcs.foreach(map => println(map._1.id, map._2.id))

      println("arcsInCycle after")
      arcsInCycle.foreach(map => println(map._1.id, map._2.id))

 */



      val resultArborescenceArcs = treeArcs.++(arcsInCycle.toList)
/*
      println("resultArborescenceArcs")
      resultArborescenceArcs.foreach(map => println(map._1.id, map._2.id))

 */

      def constructArborescence(site1:Site ,site2:Site):Boolean= {
        if (resultArborescenceArcs.contains((site1,site2))){
          true
        } else {false}
      }

      arcWise(inputPrime, constructArborescence).entries
    }

    // return the minimum cost in-site and the corresponding cost
    def inDegreeSites(graph:Map[Site, Map[Site, Double]],site:Site):(List[(Site,Site)],Map[Site,Double])={

      var result: List[(Site,Double)] = List()
      graph.collect{
        case (site1, map1) => (site1,map1.collect{
          case (site2, value) if site2 == site && site1 != site2
          => result = result ::: (site1,value) :: Nil
        }
        )
      }
      val sorted_list = result.sortBy(_._2)
      //sorted_list.foreach{e => println("insite: "+e._1.id+" with cost "+e._2)}
      val minInSite = sorted_list.head._1
      val minInCost = sorted_list.head._2
      //println("site: "+site.id, "in site: ",minInSite.id)
      (List((minInSite,site)),Map(site->minInCost))
    }



/*
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

 */

    def detectCycles(pairMap:List[(Site, Site)],input: Input):List[Tour] = {

      /*
      println("====================detect cycles=====================")
      pairMap.foreach(map => println(map._1.id, map._2.id))

       */


      var listTour: List[Tour] = List()
      var reversedArc = false

      for(arc <- pairMap){
        // println("current arc is " + arc._1.id,arc._2.id)
        if (pairMap.contains((arc._2, arc._1))){
          // println("find a reversed arc",(arc._2, arc._1))
          val cycle = new Tour(input,List(arc._2, arc._1))
          listTour = List(cycle)
          return listTour
        }
      }

      // println("didn't find a reversed arc, start DFS")

      var visited:Map[Site,Boolean] = {
        input.sites.map(site => (site -> false)).toMap
      }
      var recStack:Map[Site,Boolean] = {
        input.sites.map(site => (site -> false)).toMap
      }

      var currentTour:List[Site]=List()

      for(site <- input.sites){
        if (isCyclicUtil(pairMap,site,visited,recStack)){
          // println("=================find a tour using DFS=================",site.id)
          currentTour = site :: currentTour
        }

      }
      if(currentTour.nonEmpty) {
        val tour = new Tour(input, currentTour)
        listTour = listTour ::: tour :: Nil
      }
      listTour
    }

    /*
    def isCyclicUtil(pairMap:List[(Site, Site)],site:Site, visited:Map[Site,Boolean],recStack:Map[Site,Boolean]) : Boolean = {
      // Mark the current node as visited and
      // part of recursion stack
      if (recStack(site)) {
        return true
      }

      if (visited(site)) {
        return false
      }

      val visitedUpdated = visited.updated(site,true)
      val recStackUpdated = recStack.updated(site,true)

      val children:List[Site] = pairMap.filter(_._1 == site).collect{case (site1,site2)=>site2}
      children.collect{
        case child if isCyclicUtil(pairMap,child,visitedUpdated,recStackUpdated) => return true
      }

      val recStackUpdatedPrime = recStackUpdated.updated(site,false)

      false
    }

     */
    def isCyclicUtil(pairMap:List[(Site, Site)],site:Site, visited:Map[Site,Boolean],recStack:Map[Site,Boolean]): Boolean = {
      // Mark the current node as visited and
      // part of recursion stack
      if (recStack(site)) {
        return true
      }

      if (visited(site)) {
        return false
      }

      val visitedUpdated = visited.updated(site,true)
      val recStackUpdated = recStack.updated(site,true)

      val children:List[Site] = pairMap.filter(_._1 == site).collect{case (site1,site2)=>site2}
      children.collect{
        case child if isCyclicUtil(pairMap,child,visitedUpdated,recStackUpdated) => return true
      }

      val recStackUpdatedPrime = recStackUpdated.updated(site,false)

      false
    }

    def shrinkGraph(graph:Map[Site, Map[Site, Double]],cycle:List[Site],minCost:Map[Site,Double]):(Map[Site, Map[Site, Double]],Map[Site, Map[Site, Double]],Map[Site, Map[Site, Double]],Site)={
      // create supernode newSite
      val newSite = new Site()

      // if a node outside cycle, say u, has multiple arcs into the supernode, then just keep the minimum-cost arc
      val minToCycle:Map[Site, Map[Site,Double]]={
        var result:Map[Site, Map[Site,Double]] = Map()
        for (site <-graph.keys){
          if (!cycle.contains(site)){
            var minMap:Map[Site,Double] = Map(site->inf)
            for (siteInCycle <- cycle){
              if (graph(site)(siteInCycle) < minMap.last._2){
                minMap = Map(siteInCycle->graph(site)(siteInCycle))
              }
            }
            result = result.++(Map(site -> minMap))
          }
        }
        result
      }
/*
      println("minToCycle")
      for(site1<-minToCycle.keys){
        for (site2 <-minToCycle(site1)){
          println(site1.id,site2._1.id,site2._2)
        }
      }

 */

      // Is it necessary to keep only the min-cost out-arc of the supernode?
      // if the supernode has multiple out-arcs to a node outside the circle, then just keep the minimum-cost one
      val minFromCycle:Map[Site, Map[Site,Double]]={
        var result:Map[Site, Map[Site,Double]] = Map()
        for (site <-graph.keys){
          if (!cycle.contains(site) && site != rootSite){
            var minMap:Map[Site,Double] = Map(site->inf)
            for (siteInCycle <- cycle){
              if (graph(siteInCycle)(site) < minMap.last._2){
                minMap = Map(siteInCycle->graph(siteInCycle)(site))
              }
            }
            result = result.++(Map(site -> minMap))
          }
        }
        result
      }
/*
      println("minFromCycle")
      for(site1<-minFromCycle.keys){
        for (site2 <-minFromCycle(site1)){
          println(site1.id,site2._1.id,site2._2)
        }
      }

 */

      //
      var newMap = graph.collect{
        case (site1,map1) => (site1,map1.collect{
          // should only keep the min-cost in-arc of the supernode
          case(site2, value) if !cycle.contains(site1) && cycle.contains(site2) && value == minToCycle(site1).values.head => (newSite, value)
          case(site2, value) if !cycle.contains(site1) && !cycle.contains(site2) => (site2, value)
          case(site2, value) if cycle.contains(site1) && !cycle.contains(site2) => (site2, value)
        })
      }

      cycle.foreach(siteInCycle => newMap = newMap - siteInCycle)
      newMap = newMap + (newSite->minFromCycle.map{case (site1, map1)=>(site1,map1.values.head)})
      (newMap,minToCycle,minFromCycle,newSite)
    }

    val resultAssignment:Map[Site,Map[Site,Boolean]] = chuLiuEdmonds(costsPrime)

    var result:Double = 0.0

    resultAssignment.collect{
      case (site1,map1) => (site1, map1.collect{
        case (site2, value) if value => result = result + input.distMat(site1)(site2)
      })
    }
    /*
    println("rSAP result")
    resultAssignment.collect{
      case (site1,map1) => (site1, map1.collect{
        case (site2, value) => println(input.distMat(site1)(site2))
      })
    }
    println("rSAP result")
    resultAssignment.collect{
      case (site1,map1) => (site1, map1.collect{
        case (site2, value) if value => println(site1.id,site2.id,input.distMat(site1)(site2))
      })
    }

     */
    println("===============lower bound rSAP is: ",result,"=======================")
    result
  }

  // not used
  def computeLB (branchNode: BranchNode) : LowerBound = {
    0.0
  }








}
