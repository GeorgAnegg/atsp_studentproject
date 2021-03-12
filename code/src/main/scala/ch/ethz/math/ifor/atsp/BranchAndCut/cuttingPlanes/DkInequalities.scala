package ch.ethz.math.ifor.atsp.BranchAndCut.cuttingPlanes

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode
import ch.ethz.math.ifor.atsp.{Site, negInf}
import com.google.ortools.linearsolver.MPVariable

object DkInequalities extends CuttingPlane {
  def findCuts(branchNode:BranchNode,globalCuts:List[(Map[MPVariable,Double],Double)]):List[(Map[MPVariable,Double],Double)]= {
    //println("start dk")
    // first shrink all 1-path arcs
    // set up
    var listSites: List[Site] = branchNode.input.sites.toList

    var shrunkCostMap: Map[Site, Map[Site, Double]] = branchNode.lowerBoundSolve

    // identify all paths of 1-arcs
    var oneCostArcs: Map[Site, Site] = Map()
    shrunkCostMap.collect {
      case (site1, map1) => (site1, map1.collect {
        case (site2, value) if value == 1.0 => oneCostArcs = oneCostArcs ++ Map(site1 -> site2)
      })
    }
    var paths: List[List[(Site, Site)]] = List()

    while (oneCostArcs.nonEmpty) {
      var currentPath: List[(Site, Site)] = List((oneCostArcs.head._1,oneCostArcs.head._2))
      oneCostArcs = oneCostArcs.drop(1)

      // search for arcs adjacent to currentPath
      // search for descendants
      while (oneCostArcs.keys.toList.contains(currentPath.last._2)) {
        currentPath = currentPath :+ (currentPath.last._2, oneCostArcs(currentPath.last._2))
        oneCostArcs = oneCostArcs.removed(currentPath.last._2)
      }
      // search for precedents
      while (oneCostArcs.values.toList.contains(currentPath.head._1)) {
        currentPath = (oneCostArcs.find(_._2 == currentPath.head._1).get._1, currentPath.head._1) +: currentPath
        oneCostArcs = oneCostArcs.filter(_._2 != currentPath.head._1)
      }
      // now currentPath can not be extended, add to the paths list
      paths = paths :+ currentPath
    }

    // finish identifying all paths of 1-arcs
    // start shrinking
    var clusterNode: Map[Site, List[Site]] = Map()

    def shrinkPath(list: List[(Site, Site)]): Unit = {
      //println("=================")

      var res: List[Site] = List()
      for (i <- list) {
        res = res :+ i._2
      }
      res = res.filter(_ != list.head._1).distinct
      clusterNode = clusterNode ++ Map(list.head._1 -> res)

      for (v <- res){
        /*
        for(i<-shrunkCostMap){
          for(j<-i._2){
            println(i._1,j._1,j._2)
          }
        }

         */
        //println("size of shrunk graph: "+shrunkCostMap.keys.size,shrunkCostMap.values.size)

        // all arcs going from/in v are added to list.head._1
        shrunkCostMap = shrunkCostMap.map{
          case (site1, map1) => (site1, map1.map{
            case (site2, value) if site1 == list.head._1 => (site2, value + shrunkCostMap(v)(site2))
            case (site2, value) if site2 == list.head._1 => (site2, value + shrunkCostMap(site1)(v))
            case (site2, value) => (site2, value)
          })
        }
        listSites = listSites.filter(_ != v)
      }

      // remove all nodes in cluster
      /*
      shrunkCostMap = shrunkCostMap.collect {
        case (site1, map1) => (site1, map1.collect {
          case (site2, value) if !res.contains(site1) && !res.contains(site2) => (site2, value)
        })
      }

       */
    }

    paths.foreach{path => shrinkPath(path)}
    //println("size of list sites: "+listSites.size)
    //println("size of shrunk graph: "+shrunkCostMap.keys.size,shrunkCostMap.values.size)
    /*
    def shrinkArc(u: Site, v: Site): Unit = {
      clusterNode = clusterNode.updated(u, List.concat(clusterNode(u), clusterNode(v)))

      // all arcs going from/in v are added to u
      shrunkCostMap = shrunkCostMap.map {
        case (site1, map1) => (site1, map1.map {
          case (site2, value) if site1 == u => (site2, value + shrunkCostMap(v)(site2))
          case (site2, value) if site2 == u => (site2, value + shrunkCostMap(site1)(v))
          case (site2, value) => (site2, value)
        })
      }

      // remove v
      shrunkCostMap = shrunkCostMap.collect {
        case (site1, map1) => (site1, map1.collect {
          case (site2, value) if site1 != v && site1 != v => (site2, value)
        })
      }
      listSites = listSites.filter(_ != v)
    }

     */

    var phiList: Map[List[Site], Double] = Map()
    var piList: Map[List[Site], Double] = Map()

    def computePhi(list: List[Site]): Double = {
      if (list.size == 1) {
        phiList = phiList ++ Map(list -> 0)
        return 0
      }
      val previous = list.take(list.size - 1)
      var result = phiList(previous) - 1
      previous.foreach(site => result += shrunkCostMap(site)(previous.last))
      result -= shrunkCostMap(previous.last)(previous.last)
      result += shrunkCostMap(list.head)(list.last)
      result += shrunkCostMap(list.last)(previous.last)
      phiList = phiList ++ Map(list -> result)
      result
    }

    def computePi(list: List[Site]): Double = {
      if (list.size == 1) {
        piList = piList ++ Map(list -> 0)
        return 0
      }
      val previous = list.take(list.size - 1)
      var result = piList(previous) - 1
      previous.foreach(site => result += shrunkCostMap(site)(list.last))
      result += shrunkCostMap(list.last)(previous.last)
      piList = piList ++ Map(list -> result)
      result
    }

    /*
    branchNode.lowerBoundSolve.foreach {
      case (site1, map1) => (site1, map1.foreach {
        case (site2, value) if value == 1.0 => shrinkArc(site1, site2)
      })
    }
     */

    var refMpVariables: Map[MPVariable,Map[Map[Site,Site],Double]] = Map()

    def constructInequality(list: List[Site]): List[(Map[MPVariable, Double], Double)] = {
      var resultMap: Map[MPVariable, Double] = Map()

      val mpV1 = branchNode.variables.search(list.head, list.last)
      resultMap = resultMap ++ Map(mpV1 -> 1.0)
      refMpVariables = refMpVariables ++ Map(mpV1 -> Map(Map(list.head-> list.last)->1))

      for (i <- 2 to list.size) {
        val mp = branchNode.variables.search(list(i - 1), list(i - 2))
        resultMap = resultMap ++ Map(mp -> 1.0)
        refMpVariables = refMpVariables ++ Map(mp -> Map(Map(list(i - 1)-> list(i - 2))->1))
      }

      for (i <- 2 until list.size) {
        val mp = branchNode.variables.search(list.head, list(i - 1))
        resultMap = resultMap ++ Map(mp -> 2.0)
        refMpVariables = refMpVariables ++ Map(mp -> Map(Map(list.head-> list(i - 1))->2))
      }

      for (i <- 3 until list.size) {
        for (j <- 2 until i) {
          val mp = branchNode.variables.search(list(j - 1), list(i - 1))
          resultMap = resultMap ++ Map(mp -> 1.0)
          refMpVariables = refMpVariables ++ Map(mp -> Map(Map(list(j - 1)-> list(i - 1))->1))
        }
      }

      List((resultMap, list.size - 1))
    }

    def computeOnShrunkGraph(): List[(Map[MPVariable, Double], Double)] = {
      var phiMax: Double = 0.0
      var nodes: List[List[Site]] = List()
      listSites.foreach { site => nodes = nodes ++ List(List(site)) }
      var resultInequality: List[(Map[MPVariable, Double], Double)] = List()

      while (nodes.nonEmpty) {
        val current = nodes.head
        nodes = nodes.drop(1)
        val slack = computePhi(current) - current.size + 1
        println("Length of current dk list: ",current.length,"slack is: "+slack)
        phiList.foreach{case e => println(e._1.size,e._2)}
        if (slack > 0) {
          // if current list is violated, add to the result list
          val newInequality = constructInequality(current)
          resultInequality = resultInequality ++ newInequality
          // if slack > phiMax, update phiMax
          if (slack > phiMax) {
            phiMax = slack
          }
          if (resultInequality.size > 10) {
            // if already 10 cuts, stop
            nodes = nodes.drop(nodes.size)
          }
        } else {
          // if current list does not violate any inequality, generate all possible children
          for (i <- listSites) {
            if (!current.contains(i)) {
              // if condition satisfied
              if (shrunkCostMap(current.last)(i) > phiMax - computePi(current)) {
                val child = current ++ List(i)
                nodes = nodes ++ List(child)
              }
            }
          }
        }
      }
      resultInequality
    }

    val shrunkInequalities = computeOnShrunkGraph()

    // now we need to transform inequalities on the shrunk graph to inequalities on the original graph
    // i.e., clique lifting

    // basically we need to lift node u to cluster u = (node u and node v)

    // first compute beta_{hh} = max{ beta_{ih} + beta_{hj} - beta_{ij} : i,j in V\{h}, i\neq j}
    // for all h in shrunk graph
    var betaMap: Map[Site, Double] = Map()
    listSites.foreach { site => betaMap = betaMap + (site -> negInf) }

    for (h <- listSites) {
      for (i <- listSites) {
        for (j <- listSites) {
          if (i != h && j != h) {
            val delta = shrunkCostMap(i)(h) + shrunkCostMap(h)(j) - shrunkCostMap(i)(j)
            if (delta > betaMap(h)) {
              betaMap = betaMap.updated(h, delta)
            }
          }
        }
      }
    }

    var originalInequalities: List[(Map[MPVariable, Double], Double)] = List()

    // now for each inequality on the shrunk graph, transform it to the corresponding inequality
    // on the original graph

    for (inequality <- shrunkInequalities) {

      var resultMap = inequality._1
      var resultUpperBound = inequality._2

      // lift the left and right hand side of the inequality
      for (node <-resultMap){
        val res = refMpVariables(node._1)
        val i = res.head._1.head._1
        val j = res.head._1.head._2
        val cost = res.head._2

        for (node <- clusterNode(i)){
          resultMap = resultMap ++ Map(branchNode.variables.search(node, i) -> betaMap(i))
          resultMap = resultMap ++ Map(branchNode.variables.search(i, node) -> betaMap(i))
          resultMap = resultMap ++ Map(branchNode.variables.search(node, j) -> cost)
          resultMap = resultMap ++ Map(branchNode.variables.search(j, node) -> cost)
        }
        resultUpperBound += betaMap(i) * clusterNode(i).size
      }
      originalInequalities = originalInequalities :+ (resultMap,resultUpperBound)
    }
    println("finish dk, number of cuts found: "+originalInequalities.size)
    originalInequalities
  }
}
