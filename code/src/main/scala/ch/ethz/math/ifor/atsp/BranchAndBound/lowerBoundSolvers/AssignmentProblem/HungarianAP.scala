package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.AssignmentProblem
import ch.ethz.math.ifor.atsp.{Site, arcWise, inf}
import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, LowerBound}
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.LowerBoundSolver

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}
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

    val sitesRight: Array[Site] = {
      sitesLeft.map{site => new Site(site.id+"Right")}
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

    // construct arc between left sites set and right sites set
    val mapV1 = costs.entries.map{case(site1,map1)=>(site1,map1.map{case(site2,value)=>(searchByID(sitesRight,site2.id+"Right"),value)})}
    // block ii entries
    /*
    val mapV2 = mapV1.collect{
      case(site1,map1) => (site1, map1.updated(searchByID(sitesRight,site1.id+"Right"),inf))
    }

     */

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
    var stMap = addSToMap(start,mapV1)

    // add arcs from nodes in right sites set to t
    sitesRight.foreach{e=>
      stMap = stMap + (e -> Map(destination->0))
    }

    // construct array for all sites, i.e., s + left + right + t
    val allSites:Array[Site] = Array.concat( sitesLeft, sitesRight) :+ start :+ destination
    allSites.foreach{e=>println("all sites",e,e.id)}

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
    def updateResidualGraph(graph: Map[Site,Map[Site,Double]], arcs :Map[Site,Site], potential:Map[Site,Double]): Map[Site,Map[Site,Double]] = {
      /*
      val arc:(Site,Site)
      val result: Map[Site, Map[Site, Double]]= graph.collect{
        case (site1,map1) if site1.id == arc._1.id => (site1,map1.removed(arc._2))
        case (site1,map1) if site1.id == arc._2.id => (site1,map1+(arc._1->0.0))
        case (site1,map1) if site1.id != arc._1.id && site1.id != arc._2.id => (site1,map1)
      }
       */
      print("print arcs here")
      arcs.foreach{e=>println(e._1,e._1.id,e._2,e._2.id)}

      val result :Map[Site, Map[Site, Double]]= graph.collect{
        case (site1,map1) if arcs.contains(site1) => (site1,map1+(arcs(site1)->0.0))
        // why does this line not work?
        case (site1,map1) if arcs.values.exists(_.id == site1.id) => (site1,map1-arcs.find(_._2.id == site1.id).map(_._1).head)
        case (site1,map1) if !arcs.contains(site1) && !arcs.values.exists(_.id == site1.id) => (site1,map1)
      }

      val result2: Map[Site, Map[Site, Double]]= result.map{ case(site1,map1) => (site1,map1.map{
        case (site2,value) if arcs.exists(x => x._1 == site1 && x._2 == site2) || arcs.exists(x => x._1 == site2 && x._2 == site1) => (site2,value)
        case (site2,value) if !arcs.exists(x => x._1 == site1 && x._2 == site2) && !arcs.exists(x => x._1 == site2 && x._2 == site1) => (site2,potential(site1)+value-potential(site2))
      })
      }
      result2
    }

    // implement function to update matching
    def updateMatching(arcs:Map[Site,Site],matching:Map[Site,Site]):Map[Site,Site]={
        matching.++(arcs)
    }
    // implement Dijkstra's shortest s-t path algorithm
    def dijkstra(graph:Map[Site,Map[Site,Double]]): (Map[Site,Double],Map[Site,Site],Map[Site,Site]) = {

      // construct a map to record min-weighted predecessors of sites
      val dijkstraPre: mutable.Map[Site, Site] = mutable.Map()

      // construct the initial distance map, initially assign all to inf except 0.0 to s node
      val dijkstraDist: mutable.Map[Site, Double] = {
        allSites.zipWithIndex.map {
          case (site, _) => site -> inf
        }.to(collection.mutable.Map)
      }

      // set the distance of node s to 0.0
      dijkstraDist.update(start, 0.0)

      // construct the queue, and the labeling array
      var explored: Array[Site] = Array()
      var queue: List[(Site, Double)] = List((start, 0.0))

      // while all nodes are not visited, continue the process
      breakable {
        while (queue.nonEmpty || explored.length != allSites.length) {

          // take the first element, i.e., lowest-weighted unvisited node
          queue = queue.sortBy(_._2)
          val currentSite = queue.head
          if (currentSite._1.id=="t") {break}
          println("current site", currentSite._1, currentSite._1.id, currentSite._2)
          println("queue")
          for (i <- queue) {
            println(i._1, i._1.id, i._2)
          }

          // delete the current node from the queue and label it as visited
          queue = queue.drop(1)

          explored = explored :+ currentSite._1

          // construct all the sites that can be reached from currentSite
          val reachable = graph(currentSite._1).keys

          println("reachable")
          for (i <- reachable) {
            println(i.id, i)
          }
          println("dijkstraDist")
          dijkstraDist.foreach {
            e => println(e._1, e._1.id, e._2)
          }

          // if min-distance can be updated, update; add all these sites to the queue
          reachable.foreach { nextsite =>
            if (dijkstraDist(nextsite) > dijkstraDist(currentSite._1) + graph(currentSite._1)(nextsite)) {
              dijkstraDist.update(nextsite, dijkstraDist(currentSite._1) + graph(currentSite._1)(nextsite))
              dijkstraPre.update(nextsite, currentSite._1)
              println("add", (nextsite, nextsite.id, dijkstraDist(nextsite)))
              queue = queue ::: (nextsite, dijkstraDist(nextsite)) :: Nil
            }
          }
        }
      }

      println("dijkstraDist")
      dijkstraDist.foreach {
         e => println(e._1, e._1.id, e._2)
        }

      // construct a t-s path from dijkstraPre
      var path: Array[Site] = Array(destination)
      while (path.last.id != "s") {
        path = path :+ dijkstraPre(path.last)
      }

      path.foreach{e=>println(e,e.id)}

      val arcs: Map[Site, Site] = {
        path.zipWithIndex.collect {
          case (site, index) if index < path.length - 1 => site-> path(index + 1)
        }.toMap
      }



      val newMatching :Map[Site,Site] = {
        path.zipWithIndex.collect {
          case (site, index) if index%2==1 && index < path.length - 1 => path(index + 1)-> site
        }.toMap
      }
      print("newMatching matching")
      newMatching.foreach{e=>println(e._1,e._1.id,e._2,e._2.id)}

      println("hello")
        // return a distance map and a path map
        (dijkstraDist.toMap, arcs, newMatching)
      }

    // while the size of matching is not equal to numSite, iteratively call dijkstra after updating parameters
    while(matching.size != numSites) {


      val currentResult = dijkstra(stMap)
      val currentDistance = currentResult._1
      val currentPath = currentResult._2
      val currentMatching = currentResult._3
      // update potential
      potential = updatePotential(potential,currentDistance)
      print("current potential")
      potential.foreach{e=>println(e._1,e._1.id,e._2)}

      print("current Path")
      currentPath.foreach{e=>println(e._1,e._1.id,e._2,e._2.id)}

      // update residual map
      stMap = updateResidualGraph(stMap, currentPath, potential)

      println("current map")
      for (item <- stMap) {
        for (i <- item._2) {
          print(item._1, item._1.id, i._1, i._1.id, i._2 + "  ")
        }
        println("")
      }
      // update matching
      matching = updateMatching(currentMatching, matching)


      println("size of matching",matching.size)
      print("current matching")
      matching.foreach{e=>println(e._1,e._1.id,e._2,e._2.id)}

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
