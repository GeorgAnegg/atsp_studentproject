package ch.ethz.math.ifor.atsp.BranchAndCut.cuttingPlanes

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode
import ch.ethz.math.ifor.atsp.{Site, inf}
import com.google.ortools.linearsolver.MPVariable

import scala.collection.mutable

object MinCut extends CuttingPlane {
  def findCuts(branchNode:BranchNode,globalCuts:List[(Map[MPVariable,Double],Double)]):List[(Map[MPVariable,Double],Double)]={
    // first transform ATSP to STSP
    var costMap = branchNode.lowerBoundSolve.map {
      case (site1, map1) => (site1, map1.map {
        case (site2, value) => (site2, value + branchNode.lowerBoundSolve(site2)(site1))
      })
    }
    /*
    print("costmap\r\n")
    for (i<-costMap){
      for (j<-i._2){
        print(i._1,j._1,j._2,"\r\n")
      }
    }

     */

    // MINCUT consists of five procedures
    // 2.1 setup
    var listSites:List[Site]=branchNode.input.sites.toList
    var listEdges:List[(Site,Site)]=List()
    branchNode.lowerBoundSolve.collect{
      case (site1, map1) => map1.collect{
        case (site2, value) if value != 0 && site1 != site2 => listEdges = listEdges ++ List((site1,site2),(site2,site1))
      }
    }
    var adjcentNodeList :Map[Site, List[Site]] = Map()
    listSites.foreach(site => adjcentNodeList = adjcentNodeList ++ Map(site -> List()))
    costMap.collect{
      case (site1, map1) => map1.collect{
        case (site2, value) if value != 0 =>
          adjcentNodeList = adjcentNodeList.updated(site1, List.concat(adjcentNodeList(site1),List(site2)))
      }
    }

    //var costMap:Map[Site,Map[Site,Double]] = branchNode.lowerBoundSolve

    var clusterNode:Map[Site,List[Site]] = Map()
    listSites.foreach(site => clusterNode = clusterNode ++ Map(site -> List(site)))

    var capacityNode:Map[Site,Double] = Map()
    listSites.foreach(site => capacityNode = capacityNode ++ Map(site -> 0.0))
    costMap.foreach{
      case (site1, map1) => map1.foreach{
        case (site2, value) => capacityNode = capacityNode.updated(site1, capacityNode(site1)+value)
      }
    }
    /*
    print("capacity")
    capacityNode.foreach{
      case (site,value) => print(site,value+"\r\n")
    }

     */

    var minimumCut:List[Site]=List()
    var upperBoundMinCut = inf
    for (site <- branchNode.input.sites){
      if (upperBoundMinCut > capacityNode(site)){
        upperBoundMinCut = capacityNode(site)
        minimumCut = List(site)
      }
    }

    //print("initial upperBoundMinCut: ",upperBoundMinCut+"\r\n")

    // Implement the function shrink
    def shrink(u: Site,v:Site):Unit={
      //print("shrinking ",u," and ",v,"\r\n")
      var markNode:Map[Site,Boolean]=Map()
      var tempNode:Map[Site, Double] = Map()

      listSites.foreach(site => markNode = markNode ++ Map(site -> false))

      capacityNode = capacityNode.updated(u,capacityNode(u)+capacityNode(v)-2*costMap(u)(v))
      // in PDD 1990, it's List(v) not clusterNode(v)
      clusterNode = clusterNode.updated(u, List.concat(clusterNode(u), clusterNode(v)))

      for (node<-adjcentNodeList(u)){
        markNode = markNode.updated(node,true)
        tempNode = tempNode + (node -> costMap(u)(node))
      }

      adjcentNodeList = adjcentNodeList.updated(v, adjcentNodeList(v).filter(site => site!=u))
      adjcentNodeList = adjcentNodeList.updated(u, adjcentNodeList(u).filter(site => site!=v))
      listEdges = listEdges.filter(_!=(v,u))
      listEdges = listEdges.filter(_!=(u,v))

      if (adjcentNodeList(v).nonEmpty) {
        for (node <- adjcentNodeList(v)) {
          //print("adj node ", node, markNode(node))
          adjcentNodeList = adjcentNodeList.updated(v, adjcentNodeList(v).filter(site => site != node))
          adjcentNodeList = adjcentNodeList.updated(node, adjcentNodeList(node).filter(site => site != v))
          listEdges = listEdges.filter(_ != (v, node))
          listEdges = listEdges.filter(_ != (node, v))
          if (!markNode(node)) {
            //print("node ", node + " unmarked")
            adjcentNodeList = adjcentNodeList.updated(u, List.concat(adjcentNodeList(u), List(node)))
            adjcentNodeList = adjcentNodeList.updated(node, List.concat(adjcentNodeList(node), List(u)))
            listEdges = listEdges ++ Map(u -> node)
            listEdges = listEdges ++ Map(node -> u)
          } else {
            costMap = costMap.collect {
              case (site1, map1) => (site1, map1.collect {
                case (site2, value) if site1 == u && site2 == node => (site2, tempNode(node) + costMap(v)(node))
                case (site2, value) if site1 == node && site2 == u => (site2, tempNode(node) + costMap(v)(node))
                case (site2, value) => (site2, value)
              })
            }
          }
        }
      }

      if (upperBoundMinCut > capacityNode(u)){
        upperBoundMinCut = capacityNode(u)
        //print("capacityNode(u) here "+capacityNode(u)+"\r\n")
        //print("upperBoundMinCut here "+upperBoundMinCut+"\r\n")
        minimumCut = clusterNode(u)
      }
      // remove node v
      listSites = listSites.filter(_!=v)
    }

    // test of Corollary. 2.2, PD1990a
    def test1(shrunk:Boolean):Boolean={
      var result= shrunk
      var localEdges = listEdges
      while (localEdges.nonEmpty && !result) {
        val edge = localEdges.head
        var y = adjcentNodeList(edge._1).intersect(adjcentNodeList(edge._2))
        // if y empty, then only test the following
        if (capacityNode(edge._1) <= 2 * costMap(edge._1)(edge._2) || capacityNode(edge._2) <= 2 * costMap(edge._2)(edge._1)) {
          //print("Test1.1 passed\r\n")
          shrink(edge._1, edge._2)
          result = true
        }

        // if y not empty, then also test the condition below
        if (!result) {
          while (y.nonEmpty && !result) {
            val c1 = costMap(edge._1)(edge._2) + costMap(edge._1)(y.head)
            val c2 = costMap(edge._2)(edge._1)
            val c3 = costMap(edge._2)(edge._1) + costMap(edge._2)(y.head)
            if (capacityNode(edge._1) <= 2 * c1 || capacityNode(edge._2) <= 2 * c2 || capacityNode(edge._2) <= 2 * c3) {
              //print("Test1.2 passed\r\n")
              shrink(edge._1, edge._2)
              result = true
            } else {
              y = y.drop(1)
            }
          }
        }
        localEdges=localEdges.filter(_!=edge)
        }
      result
      }

    def min(d: Double, d1: Double):Double={
      if (d<=d1){
        d
      } else {
        d1
      }
    }

    def test2(shrunk:Boolean):Boolean={
      var result = shrunk
      var localEdges = listEdges
      while (localEdges.nonEmpty && !result) {
        val edge = localEdges.head
        var lowerBound = costMap(edge._1)(edge._2)
        val y = adjcentNodeList(edge._1).intersect(adjcentNodeList(edge._2))

        y.foreach { site => lowerBound = lowerBound + min(costMap(edge._1)(site), costMap(edge._2)(site)) }
        if (lowerBound > upperBoundMinCut) {
          //print("test2\r\n")
          shrink(edge._1, edge._2)
          result = true
        }
        localEdges=localEdges.filter(_!=edge)
      }
      result
    }

    var auCasOu:List[Site]=List()
    def maxFlow(u: Site,v:Site):Boolean={
      //println("start computing max flow between "+u+" and "+v)
      // compute the max flow f from u to v
      var flow: Double = 0.0
      //var marked:Map[Site,Boolean]=Map()
      //listSites.foreach(site => marked = marked ++ Map(site -> false))
      //marked = marked.updated(u,true)
      // Step 1: Transform the undirected graph to the corresponding directed graph

      var residualCost = costMap

      //var auCasOu:List[Site]=List()
      def hasAugmentingPathUsingDijkstra(graph:Map[Site,Map[Site,Double]]): (Map[Site, Site],Double) = {
        //print("length of list sites is: ",listSites.size+"\r\n")

        var allZero:Boolean=true
        //println("residual graph")

        for (i<-listSites){
          for (j<-listSites){
            //println(i,j,graph(i)(j))
            if (graph(i)(j)!=0){
              allZero = false
            }
          }
        }
        if (allZero){
          for (i<-listSites){
            if (!clusterNode(i).contains(v)){
              auCasOu = clusterNode(i)
            }
          }
          return (Map(),0)
        }

        /*
        println("cluster")
        for (i<-listSites){
          for (j<-clusterNode(i)){
            println(i,j,capacityNode(i))
          }
        }

         */

        // construct a map to record max-weighted predecessors of sites
        val dijkstraPre: mutable.Map[Site, Site] = mutable.Map()

        // construct the initial distance map, initially assign all to inf except 0.0 to s node
        val dijkstraDist: mutable.Map[Site, Double] = {
          listSites.zipWithIndex.map {
            case (site, _) => site -> 0.0
          }.to(collection.mutable.Map)
        }

        // set the distance of node s to 0.0
        dijkstraDist.update(u, 0.0)

        // construct the queue, and the labeling array
        var explored: Array[Site] = Array()
        var queue: List[(Site, Double)] = List((u, 0.0))

        // while all nodes are not visited, continue the process
        while (queue.nonEmpty && explored.length != listSites.length-1) {
          //print("loop")

          // take the first element, i.e., largest-weighted unvisited node
          queue = queue.sortBy(_._2).reverse
          val currentSite = queue.head

          // delete the current node from the queue and label it as visited
          queue = queue.drop(1)

          explored = explored :+ currentSite._1

          //print("currentSite ",currentSite._1+"\r\n")
          // construct all the sites that can be reached from currentSite
          val reachable = graph(currentSite._1).keys.toList.intersect(listSites)
          //print("reachable\r\n")
          /*reachable.foreach{
            site => print(site,"\r\n")
          }

           */

          // if min-distance can be updated, update; add all these sites to the queue
          reachable.foreach { nextsite =>
            if (dijkstraDist(nextsite) < dijkstraDist(currentSite._1) + graph(currentSite._1)(nextsite)) {
              //print("can now update\r\n")
              dijkstraDist.update(nextsite, dijkstraDist(currentSite._1) + graph(currentSite._1)(nextsite))
              dijkstraPre.update(nextsite, currentSite._1)
              //println("add", (nextsite.id, dijkstraDist(nextsite)))
              if (queue.exists(_._1 == nextsite)){
                queue = queue.filter(_._1!=nextsite)
              }
              if (nextsite!=v) {
                queue = queue ::: (nextsite, dijkstraDist(nextsite)) :: Nil
              }
            }
          }
        }

        //print("end of while\r\n")

        // construct a t-s path from dijkstraPre
        var path: Array[Site] = Array(v)

        // if infeasible
        if (!dijkstraPre.contains(v)){
          //print("infeasible\r\n")
          return (Map(),0.0)
        }

        while (path.last != u) {
          //println(path.last.id,path.length)
          path = path :+ dijkstraPre(path.last)
        }
        /*
        println("path here")
        path.foreach{e => println(e.id)}
         */
        path = path.reverse

        val arcs: Map[Site, Site] = {
          path.zipWithIndex.collect {
            case (site, index) if index < path.length - 1 => site-> path(index + 1)
          }.toMap
        }
        var bottleNeckCapacity:Double = inf
        for (arc <- arcs){
          if (graph(arc._1)(arc._2)<bottleNeckCapacity){
            bottleNeckCapacity = graph(arc._1)(arc._2)
          }
        }

        // return a path map
        (arcs,bottleNeckCapacity)
      }

      def updateResidualGraph(graph:Map[Site,Map[Site,Double]], arcs:Map[Site,Site], bottleNeck:Double):Map[Site,Map[Site,Double]]={
        val result :Map[Site, Map[Site, Double]]= graph.collect{
          case (site1,map1) => (site1,map1.collect{
            case (site2, value) if arcs.exists(x => x._1 == site1 && x._2 == site2) => (site2,value-bottleNeck)
            case (site2, value) if arcs.exists(x => x._2 == site1 && x._1 == site2) => (site2,value+bottleNeck)
            case (site2, value) => (site2,value)
          })
        }
        result
      }

      var result = hasAugmentingPathUsingDijkstra(residualCost)

      while (result._1.nonEmpty){
        residualCost = updateResidualGraph(residualCost,result._1,result._2)
        flow = flow + result._2
        result = hasAugmentingPathUsingDijkstra(residualCost)
      }

      if (auCasOu.nonEmpty) {
        print("there're some fractional islands\r\n")
        true
      } else {
        //print("there's no more augmenting path\r\n")
        // when there's no more augmenting path, compute all reachable nodes from u in the residual graph
        def reachableFromOneNode(u: Site, graph: Map[Site, Map[Site, Double]]): List[Site] = {
          //println("start computing reachable from "+u)
          var result: List[Site] = List()
          var queue: List[Site] = List(u)
          var visited: List[Site] = List()
          while (queue.nonEmpty) {
            //println("loop", "size of queue ",queue.size)
            val current = queue.head
            visited = visited ++ List(current)
            queue = queue.drop(1)
            graph(current).collect {
              case (site2, value) if value != 0 && !queue.contains(site2) && !visited.contains(site2) => queue = queue ++ List(site2)
              case (site2, value) if value != 0 && !queue.contains(site2) && !visited.contains(site2) => result = result ++ List(site2)
            }
          }
          result
        }
        //print("flow here is: ", flow, " upperBoundMinCut: ", upperBoundMinCut + "\r\n")
        if (flow < upperBoundMinCut) {
          upperBoundMinCut = flow
          //println("upperBoundMinCut ",upperBoundMinCut)
          // minimumCut = minimum(u,v)-cut, which is computed by using Sleator-Tarjan Algorithm
          // proposed in Tarjan(1983).
          //val set1 = reachableFromOneNode(u, residualCost)
          //println("size of set1",set1.size,set1.head)
          minimumCut = clusterNode(u)
          auCasOu = clusterNode(u)
          true
        } else {
          false
        }
      }
  }

    // Implement algorithm MIN_CUT
    while (listSites.size>1){

      /*
      print("size of nodes: "+listSites.size+"\r\n")
      listSites.foreach{
        site => print(site,capacityNode(site)+"\r\n")
      }
      listEdges.foreach{
        edge => print(edge._1,edge._2+"\r\n")
      }
      listSites.foreach{
        site => adjcentNodeList(site).foreach(adjNode => print(site, "adj: " + adjNode + "\r\n"))
      }
      listSites.foreach{
        site => clusterNode(site).foreach(adjNode => print(site, "cluster: " + adjNode + "\r\n"))
      }

       */

      //minimumCut.foreach(cut => print("min cut " + cut + "\r\n"))

      if (listEdges.isEmpty){
        var resultMap:Map[MPVariable,Double] = Map()
        val set2 = branchNode.input.sites.toList diff minimumCut
        for (node1<-minimumCut){
          for (node2 <- set2){
            resultMap = resultMap ++ Map(branchNode.variables.search(node1,node2) -> -1.0)
            resultMap = resultMap ++ Map(branchNode.variables.search(node2,node1) -> -1.0)
          }
        }
        return List((resultMap,-2.0))
      }

      var shrunk = false
      //print("start test 1 \r\n")
      shrunk = test1(shrunk)

      //print("complete test 1, shrunk is ",shrunk+" \r\n")

      if (upperBoundMinCut < 2.0) {
        var resultMap: Map[MPVariable, Double] = Map()
        val set2 = branchNode.input.sites.toList diff minimumCut
        for (node1 <- minimumCut) {
          for (node2 <- set2) {
            resultMap = resultMap ++ Map(branchNode.variables.search(node1, node2) -> -1.0)
            resultMap = resultMap ++ Map(branchNode.variables.search(node2, node1) -> -1.0)
          }
        }
        return List((resultMap, -2.0))
      }

      if (!shrunk) {
        //print("test 1 fails\r\n",shrunk)
        shrunk = test2(shrunk)
      }

      //print("complete test 2, shrunk is ",shrunk+" \r\n")

      if (upperBoundMinCut < 2.0) {
        var resultMap: Map[MPVariable, Double] = Map()
        val set2 = branchNode.input.sites.toList diff minimumCut
        for (node1 <- minimumCut) {
          for (node2 <- set2) {
            resultMap = resultMap ++ Map(branchNode.variables.search(node1, node2) -> -1.0)
            resultMap = resultMap ++ Map(branchNode.variables.search(node2, node1) -> -1.0)
          }
        }
        return List((resultMap, -2.0))
      }

      if (!shrunk){
        //print("start maxFlow\r\n")
        val edge = listEdges.head
        val findCut = maxFlow(edge._1,edge._2)
        if (findCut){
          var resultMap: Map[MPVariable, Double] = Map()
          val set2 = branchNode.input.sites.toList diff auCasOu
          for (node1 <- auCasOu) {
            for (node2 <- set2) {
              resultMap = resultMap ++ Map(branchNode.variables.search(node1, node2) -> -1.0)
              resultMap = resultMap ++ Map(branchNode.variables.search(node2, node1) -> -1.0)
            }
          }
          return List((resultMap, -2.0))
        }
        shrink(edge._1,edge._2)
      }

      if (upperBoundMinCut < 2.0) {
        var resultMap: Map[MPVariable, Double] = Map()
        val set2 = branchNode.input.sites.toList diff minimumCut
        for (node1 <- minimumCut) {
          for (node2 <- set2) {
            resultMap = resultMap ++ Map(branchNode.variables.search(node1, node2) -> -1.0)
            resultMap = resultMap ++ Map(branchNode.variables.search(node2, node1) -> -1.0)
          }
        }
        return List((resultMap, -2.0))
      }
    }

    if (upperBoundMinCut < 2.0) {
      var resultMap: Map[MPVariable, Double] = Map()
      val set2 = branchNode.input.sites.toList diff minimumCut
      for (node1 <- minimumCut) {
        for (node2 <- set2) {
          resultMap = resultMap ++ Map(branchNode.variables.search(node1, node2) -> -1.0)
          resultMap = resultMap ++ Map(branchNode.variables.search(node2, node1) -> -1.0)
        }
      }
      List((resultMap, -2.0))
    } else {
    List()
    }
  }
}
