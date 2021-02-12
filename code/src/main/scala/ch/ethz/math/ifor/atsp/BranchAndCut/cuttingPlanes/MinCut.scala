package ch.ethz.math.ifor.atsp.BranchAndCut.cuttingPlanes

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode
import ch.ethz.math.ifor.atsp.{Site, inf}
import com.google.ortools.linearsolver.MPVariable

object MinCut {
  def minCut(branchNode:BranchNode):List[(Map[MPVariable,Double],Double)]={
    // MINCUT consists of five procedures
    // 2.1 setup
    var listSites:List[Site]=branchNode.input.sites.toList
    var listEdges:List[(Site,Site)]=List()
    branchNode.lowerBoundSolve.foreach{
      case (site1, map1) => map1.foreach{
        case (site2, value) if value != 0 => listEdges = listEdges ::: (site1,site2)::Nil
      }
    }
    var costMap:Map[Site,Map[Site,Double]] = branchNode.lowerBoundSolve

    var clusterNode:Map[Site,List[Site]] = Map()
    listSites.foreach(site => clusterNode = clusterNode ++ Map(site -> List(site)))

    var capacityNode:Map[Site,Double] = Map()
    listSites.foreach(site => capacityNode = capacityNode ++ Map(site -> 0.0))
    branchNode.lowerBoundSolve.foreach{
      case (site1, map1) => map1.foreach{
        case (site2, value) => capacityNode.updated(site1, capacityNode(site1)+value)
      }
    }

    var adjcentNodeList :Map[Site, List[Site]] = Map()
    listSites.foreach(site => adjcentNodeList = adjcentNodeList ++ Map(site -> List()))
    branchNode.lowerBoundSolve.foreach{
      case (site1, map1) => map1.foreach{
        case (site2, value) if value != 0 =>
          //adjcentNodeList = adjcentNodeList.updated(site1, adjcentNodeList(site1)::List(site2))
      }
    }

    var minimumCut:List[Site]=List()
    var upperBoundMinCut = inf
    for (site <- branchNode.input.sites){
      if (upperBoundMinCut > capacityNode(site)){
        upperBoundMinCut = capacityNode(site)
        minimumCut = List(site)
      }
    }

    while (listSites.size>1){
      var shrunk = false
      shrunk = test1(shrunk)
      if (!shrunk) {
        shrunk = test2(shrunk)
      }
      if (!shrunk){
        val edge = listEdges.head
        maxFlow(edge._1,edge._2)
        shrink(edge._1,edge._2)
      }
    }

    def shrink(u: Site,v:Site):Unit={
      var markNode:Map[Site,Boolean]=Map()
      var tempNode:Map[Site, Double] = Map()

      capacityNode = capacityNode.updated(u,capacityNode(u)+capacityNode(v)-2*branchNode.lowerBoundSolve(u)(v))
      //clusterNode = clusterNode.updated(u, clusterNode(u)::List(v))

      for (node<-adjcentNodeList(u)){
        markNode = markNode.updated(node,true)
        tempNode = tempNode + (node -> branchNode.lowerBoundSolve(u)(node))
      }

      for (node<-adjcentNodeList(v)){
        adjcentNodeList = adjcentNodeList.updated(v, adjcentNodeList(v).filter(site => site!=node))
        adjcentNodeList = adjcentNodeList.updated(node, adjcentNodeList(node).filter(site => site!=v))
        if (!markNode(node)){
          //adjcentNodeList = adjcentNodeList.updated(u, adjcentNodeList(u)::List(node))
          //adjcentNodeList = adjcentNodeList.updated(node, adjcentNodeList(node)::List(u))
        } else {
          costMap = costMap.collect{
            case (site1, map1)=> (site1,map1.collect{
              case (site2, value) if site1 == u && site2==node => (site2,tempNode(node) + costMap(v)(node))
              case (site2, value) => (site2, value)
            })
          }
        }
      }

      if (upperBoundMinCut > capacityNode(u)){
        upperBoundMinCut = capacityNode(u)
        minimumCut = clusterNode(u)
      }
    }

    def test1(shrunk:Boolean):Boolean={
      var result= shrunk
      val edge = listEdges.head
      val y = adjcentNodeList(edge._1).intersect(adjcentNodeList(edge._2))

      if (capacityNode(edge._1) <= 2 * costMap(edge._1)(edge._2) || capacityNode(edge._2) <= 2 * costMap(edge._2)(edge._1)){
        shrink(edge._1,edge._2)
        result = true
      }

      if (y.nonEmpty){
        val c1 = costMap(edge._1)(edge._2) + costMap(edge._1)(y.head)
        val c2 = costMap(edge._2)(edge._1)
        if (capacityNode(edge._1) <= 2 * c1 || capacityNode(edge._2) <= 2 * c2){
          shrink(edge._1,edge._2)
          result = true
        }
      }

      if (!result){
        test1(result)
      }
      result
    }

    def test2(shrunk:Boolean):Boolean={
      var result = shrunk
      val edge = listEdges.head
      var lowerBound = costMap(edge._1)(edge._2)
      val y = adjcentNodeList(edge._1).intersect(adjcentNodeList(edge._2))

      y.foreach{site => lowerBound = lowerBound + costMap(edge._1)(site) + costMap(edge._2)(site)}
      if (lowerBound > upperBoundMinCut) {
        shrink(edge._1,edge._2)
        result = true
      }

      if (!result){
        test2(result)
      }
      result
    }

    def maxFlow(u: Site,v:Site):Unit={
      // compute the max flow f from u to v
      val flow:Double = 0.0

      if (flow < upperBoundMinCut){
        upperBoundMinCut = flow
        // minimumCut = minimum(u,v)-cut, which is computed by using Sleator-Tarjan Algorithm
        // proposed in Tarjan(1983).
      }
    }

    List()
  }

}
