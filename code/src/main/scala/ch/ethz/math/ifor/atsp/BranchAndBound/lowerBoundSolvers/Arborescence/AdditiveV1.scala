package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.Arborescence

import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, LowerBound}
import ch.ethz.math.ifor.atsp.{Input, Site, Tour, arcWise, inf}

object AdditiveV1 {
/*
  def compute(input: Input,branchNode: BranchNode):Double={

    // construct reduced cost matrix
    var reducedCost = input.distMat

    // choose the root node
    // either choose the first one
    // or choose the one with outgoing arcs of minimum sum
    val rootNode = input.sites.head

    // remove all incoming arcs of root node
    input.sites.foreach{
      case site if site != rootNode => reducedCost = reducedCost.updated(site,reducedCost(site)-rootNode)
    }

    // need a function which takes site and graph as inputs and returns the minimum incoming cost
    // List((siteFrom, siteTo)), Map[siteTo,Double]
    def findMinIncomingArc(site: Site, graph:Map[Site,Map[Site,Double]]):(List[(Site,Site)],Map[Site,Double])={
      // do we need consider the case all incoming arcs are of cost inf?
      var minCost = inf
      var minArc:List[(Site,Site)] = List()
      for (i <- input.sites){
        val currentCost = graph(i)(site)
        if (currentCost < minCost){
          minCost = currentCost
          minArc = List((i,site))
        }
      }
      (minArc, Map(site->minCost))
    }

    def isCyclicUtil(i: Int, visited: Array[Boolean], recStack: Array[Boolean]): Boolean = { // Mark the current node as visited and
      // part of recursion stack
      if (recStack(i)) return true
      if (visited(i)) return false
      visited(i) = true
      recStack(i) = true
      val children = adj.get(i)
      for (c <- children) {
        if (isCyclicUtil(c, visited, recStack)) return true
      }
      recStack(i) = false
      false
    }

    // need a function that takes arcs as input and return a cycle if exists
    def isCyclic(graph:Map[Site,Map[Site,Double]]): Boolean = {
      // Mark all the vertices as not visited and
      // not part of recursion stack
      val visited = new Array[Boolean](V)
      val recStack = new Array[Boolean](V)
      // Call the recursive helper function to
      // detect cycle in different DFS trees
      var i = 0
      while ( {
        i < V
      }) {
        if (isCyclicUtil(i, visited, recStack)) return true
        i += 1
      }
      false
    }

    def Edmonds(graph:Map[Site,Map[Site,Double]]):Map[Site,Map[Site,Boolean]]={

      // for each node except root node, choose an arc of minimum incoming cost
      var minCost:Map[Site,Double]=Map()
      var minInDegreeMap:List[(Site,Site)]=List()

      // pay attention to sites here, not the same as in input, perhaps contain supernodes
      for(site <- graph.keys){
        if (site != rootNode){
          minInDegreeMap= minInDegreeMap.++(findMinIncomingArc(site,graph)._1)
          minCost= minCost.++(findMinIncomingArc(site,graph)._2)
        }
      }

      // test if minInDegreeMap contains a cycle







    }







    // first check var assignment

  }

  // not used
  def computeLB (branchNode: BranchNode) : LowerBound = {
    0.0
  }




 */







}
