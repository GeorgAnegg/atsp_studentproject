package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.AssignmentProblem
import ch.ethz.math.ifor.atsp.{Site, arcWise, inf}
import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, LowerBound}
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.LowerBoundSolver

import scala.collection.mutable
object HungarianAP extends LowerBoundSolver{

  def compute(branchNode: BranchNode): Map[Site, Map[Site, Boolean]]  = {

    val numSites = branchNode.varAssignment.size

    // TODO: implement the O(n^3) version Hungarian Method


    // create s, t nodes
    val start : Site = new Site("s")
    val destination : Site = new Site("t")


    // construct cost map
    val costs:arcWise[Double] = arcWise(branchNode.input,branchNode.input.distance)

    // construct left sites set
    val sitesLeft: Array[Site] = costs.entries.keys.toArray
    sitesLeft.foreach{e=>println("left sites",e,e.id)}

    // construct arc between left sites set and right sites set
    val mapV1 = costs.entries.map{case(site1,map1)=>(site1,map1.map{case(site2,value)=>(new Site(site2.id+"Right"),value)})}

    // construct right sites set
    // val sitesRight: Array[Site] = mapV1(sitesLeft(0)).keys.toArray
    //sitesRight.foreach{e=>println("right sites",e,e.id)}


    // add arcs from s to nodes in left sites set
    def addSToMap(site:Site, originMap:Map[Site,Map[Site, Double]]): Map[Site,Map[Site, Double]] = {
      val additionalS = sitesLeft.map{
        item => item->0.0
      }.toMap
      originMap.++(Map(site -> additionalS))
    }
    val sMap = addSToMap(start,mapV1)

    // add arcs from nodes in right sites set to t
    def addTToMap(site:Site, originMap:Map[Site,Map[Site, Double]]): Map[Site,Map[Site, Double]] = {
      originMap.collect{
        case (site1,map1) if site1.id!="s" => (site1,map1+(site->0.0))
        case (site1,map1) if site1.id=="s" => (site1,map1)
      }
    }

    var stMap = addTToMap(destination,sMap)

    val sitesRight: Array[Site] = mapV1(sitesLeft(0)).keys.toArray
    sitesRight.foreach{e=>println("right sites",e,e.id)}

    // construct array for all sites, i.e., s + left + right + t
    val allSites:Array[Site] = Array.concat( sitesLeft, sitesRight) :+ start :+ destination
    allSites.foreach{e=>println("all sites",e,e.id)}

    println("st map outside")
    for(item <-stMap){
      for(i<-item._2){
        print(item._1,item._1.id,i._1,i._1.id,i._2+"  ")
      }
      println("")
    }

    // construct potential map
    var potential : Map[Site,Double] = {
      allSites.map{
         site => site->0.0
      }.toMap
    }

    // construct matching map
    var matching : Map[Site,Site] = Map()

    // implement function to update potential
    def updatePotential(potential:Map[Site,Double],dijkstraDistance:Map[Site,Double]): Map[Site,Double]={
      potential.map{case (site,value) => (site,dijkstraDistance(site)+value)}
    }

    // implement function to update residual graph
    def updateResidualGraph(graph: Map[Site,Map[Site,Double]], arcs :Map[Site,Site]): Map[Site,Map[Site,Double]] = {
      /*
      val arc:(Site,Site)
      val result: Map[Site, Map[Site, Double]]= graph.collect{
        case (site1,map1) if site1.id == arc._1.id => (site1,map1.removed(arc._2))
        case (site1,map1) if site1.id == arc._2.id => (site1,map1+(arc._1->0.0))
        case (site1,map1) if site1.id != arc._1.id && site1.id != arc._2.id => (site1,map1)
      }
       */

      val result :Map[Site, Map[Site, Double]]= graph.collect{
        //case (site1,map1) if arcs.contains(site1) => (site1,map1.removed(arcs(site1)))
        case (site1,map1) if arcs.contains(site1) => (site1,map1+(arcs(site1)->0.0))
        case (site1,map1) if arcs.values.exists(_.id == site1.id) => (site1,map1.removed(site1))
        //case (site1,map1) if arcs.values.exists(_.id == site1.id) => (site1,map1+(arcs.find(_._2 == site1).map(_._1).head->0.0))
        case (site1,map1) if !arcs.contains(site1) && !arcs.values.exists(_.id == site1.id) => (site1,map1)
      }
      result
    }

    // implement function to update matching
    def updateMatching(arcs:Map[Site,Site],matching:Map[Site,Site]):Map[Site,Site]={
        val result = arcs.zipWithIndex.map{
          case((site1,site2),index) if index % 2 == 1 && index != 0 => site2->site1
        }.toMap
        matching.++(result)
    }
    // implement Dijkstra's shortest s-t path algorithm
    def dijkstra(graph:Map[Site,Map[Site,Double]]): (Map[Site,Double],Map[Site,Site]) = {

      println("st map inside")
      for(item <-graph){
        for(i<-item._2){
          print(item._1,item._1.id,i._1,i._1.id,i._2+"  ")
        }
        println("")
      }

      // construct a map to record min-weighted predecessors of sites
      val dijkstraPre: mutable.Map[Site, Site] = mutable.Map()

      // construct the initial distance map, initially assign all to inf except 0.0 to s node
      val dijkstraDist: mutable.Map[Site, Double] = {
        allSites.zipWithIndex.map {
          case (site, _) => site -> inf
        }.to(collection.mutable.Map)
      }

      // set the distance of node s to 0.0
      dijkstraDist.update(start,0.0)
      println("dijkstraDist")
      dijkstraDist.foreach{
        e => println(e._1,e._1.id,e._2)
      }

      // construct the queue, and the labeling array
      var explored :Array[Site] =Array()
      var queue : List[(Site,Double)] = List((start,0.0))

      // while all nodes are not visited, continue the process
      while (queue.nonEmpty || explored.length != allSites.length){

        // take the first element, i.e., lowest-weighted unvisited node
        queue = queue.sortBy(_._2)
        val currentSite = queue.head
        println("current site",currentSite._1,currentSite._1.id,currentSite._2)
        println("queue")
        for (i<-queue){
          println(i._1,i._1.id,i._2)
        }

        // delete the current node from the queue and label it as visited
        queue = queue.drop(1)

        explored = explored :+ currentSite._1

        println("st map inside2")
        for(item <-stMap){
          for(i<-item._2){
            print(item._1,item._1.id,i._1,i._1.id,i._2+"  ")
          }
          println("")
        }

        // construct all the sites that can be reached from currentSite
        val reachable = graph(currentSite._1).keys

        /*
        println("reachable")
        for (i<-reachable){
          println(i.id,i)
        }
        println("dijkstraDist")
        dijkstraDist.foreach{
          e => println(e._1,e._1.id,e._2)
        }
        println("reachable?")
        for (i<-reachable){
          println(currentSite._1.id,i.id,i,dijkstraDist(i))
          println(currentSite._1.id,dijkstraDist(currentSite._1))
          println(graph(currentSite._1)(i))
        }
        
         */

        // if min-distance can be updated, update; add all these sites to the queue
        reachable.foreach{ nextsite =>
          if (dijkstraDist(nextsite) > dijkstraDist(currentSite._1) + graph(currentSite._1)(nextsite)){
            dijkstraDist.update(nextsite,dijkstraDist(currentSite._1) + graph(currentSite._1)(nextsite))
            dijkstraPre.update(nextsite,currentSite._1)
          }
          println("add",(nextsite,nextsite.id,dijkstraDist(nextsite)))
          queue = queue ::: (nextsite,dijkstraDist(nextsite)) :: Nil
        }
      }
      println("dijkstraDist",dijkstraDist)


      // construct a t-s path from dijkstraPre
      var path : Array[Site] = Array(destination)
      while(path.last.id!="s"){
        path = path :+ dijkstraPre(path.last)
      }

      val arcs :Map[Site,Site] = {
        path.zipWithIndex.map{
          case (site,index) if index < path.length-1 => (site,path(index+1))
        }.toMap
      }
      // return a distance map and a path map
      (dijkstraDist.toMap,arcs)
    }

    // while the size of matching is not equal to numSite, iteratively call dijkstra after updating parameters
    while(matching.size != numSites) {

      val currentResult = dijkstra(stMap)
      val currentDistance = currentResult._1
      val currentPath = currentResult._2
      // update residual map
      stMap = updateResidualGraph(stMap, currentPath)
      // update matching
      matching = updateMatching(currentPath, matching)
      // update potential
      potential = updatePotential(potential,currentDistance)

    }

    // implement a function to search a site by id among all sites
    def searchByID(sites:Array[Site],identity:String):Site={
      var result: Site = null
      sites.foreach{
        site => if (site.id == identity) result = site
      }
      if(result==null){println("No such sites.")}
      result
    }

    // construct final matching
    val finalMatching = matching.map{
      case (site1,site2) => (site1, searchByID(sitesLeft,site2.id.replace("Right","")))
    }

    // construct the result assignment data structure
    def constructResult(site1:Site ,site2:Site):Boolean= {
      if (finalMatching(site1).id==site2.id) {true}
      else {false}
    }
    val resultAssignment: arcWise[Boolean] = arcWise(branchNode.input, constructResult)
    resultAssignment.entries


    // 1. set p,residual cost
    // 2. apply dij
    // 3. find shortest s-t path, add to Matching M
    // 4. update p and residual cost
    // 5. apply dij until |M| = n

    // dij takes a residual graph, returns d(i) for all i
  }

  // not used
  def computeLB (branchNode: BranchNode) : LowerBound = {
    0.0
  }

}
