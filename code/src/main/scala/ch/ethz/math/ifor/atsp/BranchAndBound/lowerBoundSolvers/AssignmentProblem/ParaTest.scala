package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.AssignmentProblem

import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, LowerBound}
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.LowerBoundSolver
import ch.ethz.math.ifor.atsp.{Site, arcWise, inf}

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

object ParaTest extends LowerBoundSolver{
  def compute(branchNode: BranchNode): (Map[Site, Map[Site, Boolean]],Map[Site, Map[Site, Double]])  = {
    // need a residual graph
    // delete newly excluded arc from the matching of parent node
    val arcExcluded : Map[Site,Site] = branchNode.excludedArcAdded
    //println("s t:",arcExcluded.head._1.id,arcExcluded.head._2.id)
    // need to delete (s,t) arc on the residual graph
    var matching : Map[Site,Site] = Map()
    for (i <- branchNode.input.sites){
      for (j <- branchNode.input.sites){
        if (branchNode.parentNode.lowerBoundSolve(i)(j)){
          matching = matching ++ Map(i->j)
        }
      }
    }
    /*
    branchNode.parentNode.lowerBoundSolve.foreach{
      case (site1, map1) => (site1, map1.foreach{
        case (site2, value) if value => matching = matching ++ Map(site1->site2)
      })
    }
     */
    matching = matching.filter(_!=arcExcluded.head)

    var included : Map[Site,Site] = Map()
    //var excluded : List[(Site,Site)] = List()
    var excluded : Map[Site,Site] = Map()
    /*
    for (map1 <- branchNode.varAssignment){
      for (map2 <- map1._2){
        if (map2._2 != null) {
          if (map2._2.get) {included = included + (map1._1 -> map2._1)}
          else {excluded= excluded :+ (map1._1 ,map2._1)}
        }
      }
    }
     */
    branchNode.varAssignment.foreach{
      case (site1, map1) => map1.foreach{
        case (site2, value) if value == null =>
        case (site2, value) if value.get => included = included + (site1 -> site2)
        //case (site2, value) if !value.get => excluded= excluded :+ (site1 ,site2)
        case (site2, value) if !value.get => excluded= excluded + (site1 ->site2)
      }
    }
    /*

    println("=========== matching==========")
    matching.foreach{
      case (site1, site2) => println(site1.id,site2.id)
    }
    println("=========== excluded==========")
    excluded.foreach{
      case (site1, site2) => println(site1.id,site2.id)
    }
    println("=========== included==========")
    included.foreach{
      case (site1, site2) => println(site1.id,site2.id)
    }

     */


    def constructCost(site1:Site ,site2:Site):Double= {
      branchNode.parentNode.reducedCostMatrixAfterAP(site1)(site2)
    }


    // implement a function to search a site by id among all sites
    def searchByID(sites:Array[Site],identity:String):Site={
      var result: Site = null
      sites.foreach{
        site => if (site.id == identity) result = site
      }
      if(result==null){
        matching.keys.foreach{
          site => if (site.id == identity) result = site
        }
        matching.values.foreach{
          site => if (site.id == identity) result = site
        }
      }
      if (result==null){
        //print("Did not find: ",identity)
      }
      result
    }

    // construct reduced cost map
    val costs:arcWise[Double] = arcWise(branchNode.input,constructCost)

    /*
    println("reduced cost of parent:")
    costs.entries.foreach{
      case (site1, map1) => (site1, map1.foreach{
        case (site2, value) => println(site1.id,site2.id,value)
      } )
    }

     */

    // construct left sites set
    val sites: Array[Site] = costs.entries.keys.toArray
    val sitesLeft = sites.filterNot(included.keys.toList.contains(_))
    var sitesRight = sites.filterNot(included.values.toList.contains(_))
    sitesRight = sitesRight.map{site => new Site(site.id+"Right")}
    /*
    println("=========== sitesLeft==========")
    sitesLeft.foreach{
      case (site1) => println(site1.id)
    }
    println("=========== sitesRight==========")
    sitesRight.foreach{
      case (site1) => println(site1.id)
    }

     */
    val start : Site = arcExcluded.head._1
    val destination : Site = searchByID(sitesRight,arcExcluded.head._2.id+"Right")
    //println("s,t :",start.id,destination.id)

    //val residualCost : Map[Site,Map[Site,Double]] = Map()


    // construct arc between left sites set and right sites set
    var stMap: Map[Site, Map[Site, Double]] = costs.entries.collect {
      case (site1, map1) => (site1, map1.collect {
        case (site2, value) if !included.keys.toList.contains(site1) && !included.exists(_._2==site2)
          && branchNode.varAssignment(site1)(site2) == null && !matching.exists(_==(site1->site2))=> (searchByID(sitesRight, site2.id + "Right"), value)
      })
    }
    matching.foreach{e=>
      stMap = stMap + (searchByID(sitesRight,e._2.id+"Right") -> Map(e._1->0))
    }

    /*
        println("======stmap in Parametric AP=====")
        stMap.foreach{
          case (site1, map1) if site1!=null => (site1, map1.foreach{
            case (site2, value) => println(site1.id,site2.id,value)
          } )
        }
        println("=======================================================")

     */


    val allSites:Array[Site] = Array.concat( sitesLeft, sitesRight) :+ start :+ destination

    // construct potential map
    var potential : Map[Site,Double] = {
      allSites.map{
        site => site->0.0
      }.toMap
    }
    // implement function to update potential
    def updatePotential(potential:Map[Site,Double],dijkstraDistance:Map[Site,Double]): Map[Site,Double]={
      potential.map{case (site,value) => (site,dijkstraDistance(site)+value)}
    }

    /*
    // implement function to update residual graph
    def updateResidualGraph(graph: Map[Site,Map[Site,Double]], arcs :Map[Site,Site], currentDistance:Map[Site,Double]): Map[Site,Map[Site,Double]] = {
      val result :Map[Site, Map[Site, Double]]= graph.collect{
        case (site1,map1) if site1 != start && site1 != destination && arcs.contains(site1)
        => (site1,map1+(arcs(site1)->0.0)-arcs.find(_._2.id == site1.id).get._1)
        case (site1,map1) if site1 == start => (site1,map1-arcs.find(_._2.id == site1.id).get._1)
        case (site1,map1) if site1 == destination =>(site1,map1+(arcs(site1)->0.0))
        case (site1,map1) => (site1,map1)
      }
      val result2: Map[Site, Map[Site, Double]]= result.map{ case(site1,map1) => (site1,map1.map{
        case (site2,value) if arcs.exists(x => x._1 == site1 && x._2 == site2) || arcs.exists(x => x._1 == site2 && x._2 == site1) => (site2,value)
        case (site2,value) if !arcs.exists(x => x._1 == site1 && x._2 == site2) && !arcs.exists(x => x._1 == site2 && x._2 == site1)
          && site2 != destination=> (site2,currentDistance(site1)+value-currentDistance(site2))
        case (site2,_) if !arcs.exists(x => x._1 == site1 && x._2 == site2) && !arcs.exists(x => x._1 == site2 && x._2 == site1) && site2 == destination=> (site2,0)
      })
      }
      result2
    }
     */

    // implement Dijkstra's shortest s-t path algorithm
    def dijkstra(graph:Map[Site,Map[Site,Double]]): (Map[Site,Double],Map[Site,Site],Map[Site,Site]) = {
      // construct a map to record min-weighted predecessors of sites
      //val dijkstraPre: mutable.Map[Site, Site] = mutable.Map()
      var dijkstraPre: Map[Site, Site] = Map()

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
      var count = 0

      // while all nodes are not visited, continue the process
      while (queue.nonEmpty && explored.length != allSites.length-1) {
        count  += 1
        // take the first element, i.e., lowest-weighted unvisited node
        queue = queue.sortBy(_._2)
        val currentSite = queue.head
        breakable {
          if (currentSite._1 == searchByID(sitesRight,destination.id)) break
        }
        // delete the current node from the queue and label it as visited
        queue = queue.drop(1)

        explored = explored :+ currentSite._1

        // construct all the sites that can be reached from currentSite
        /*
        println("current site is: ",currentSite._1.id,currentSite._2,explored.length,queue.length)
        println("=========== explored==========")
        excluded.foreach{
          case (site1, site2) => println(site1.id,site2.id)
        }
        println("=========== queue==========")
        included.foreach{
          case (site1, site2) => println(site1.id,site2.id)
        }

         */
        val reachable = graph(currentSite._1).keys

        // if min-distance can be updated, update; add all these sites to the queue
        reachable.foreach { nextsite =>
          if (dijkstraDist(nextsite) > dijkstraDist(currentSite._1) + graph(currentSite._1)(nextsite)) {
            dijkstraDist.update(nextsite, dijkstraDist(currentSite._1) + graph(currentSite._1)(nextsite))
            //dijkstraPre.update(nextsite, currentSite._1)
            dijkstraPre = dijkstraPre ++ Map(nextsite-> currentSite._1)
            //println("add", (nextsite.id, dijkstraDist(nextsite)))
            if (queue.exists(_._1 == nextsite)) {
              queue = queue.filter(_._1 != nextsite)
            }
            if (nextsite.id != destination.id) {
              queue = queue ::: (nextsite, dijkstraDist(nextsite)) :: Nil
            }
          }
        }

      }
      if (dijkstraDist(destination)<0){
        //println("No s-t path find")
        //println("infeasible")
        return (dijkstraDist.toMap, Map(), Map())
      }
      // construct a t-s path from dijkstraPre
      var path: Array[Site] = Array(destination)

      // if infeasible
      if (!dijkstraPre.contains(destination)){
        //println("infeasible")
        return (dijkstraDist.toMap, Map(), Map())
      }

      while (path.last != start) {
        //println("path last, start",start.id,path.length,dijkstraPre.size,dijkstraPre.values.toList.contains(start),dijkstraDist(destination))
        //path.foreach{e => print(e.id+"  ")}
        path = path :+ dijkstraPre(path.last)
      }

      var augPath: Map[Site, Site] = Map()
      for (i <- 1 until path.length){
        if (i%2==1) {
          augPath = augPath ++ Map(path(i) -> path(i - 1))
        }
      }

      /*
      println("augPath")
      augPath.foreach{
        e => println(e._1.id,e._2.id)
      }

       */
      val arcs: Map[Site, Site] = {
        path.zipWithIndex.collect {
          case (site, index) if index < path.length - 1 => site-> path(index + 1)
        }.toMap
      }
      // return a distance map and a path map
      (dijkstraDist.toMap, arcs, augPath)
    }

    // found a s-t path, now add it to matching
    val currentResult = dijkstra(stMap)
    val currentDistance = currentResult._1
    val currentPath = currentResult._2
    val currentMatching = currentResult._3

    // if current AP is infeasible
    if (currentPath.isEmpty){
      //println("No s-t path find")
      val assignmentInfeasible : Map[Site,Map[Site,Boolean]] = branchNode.varAssignment.map{
        case (site1, map1) => (site1, map1.map{
          case (site2, value) if site1==site2 => (site2,true)
          case (site2, value) if site1!=site2 => (site2,false)
        })
      }

      val costInfeasible : Map[Site,Map[Site,Double]] = branchNode.costsMap.map{
        case (site1, map1) => (site1, map1.map{
          case (site2, value) => (site2,inf)
        })
      }
      return (assignmentInfeasible,costInfeasible)
    }
    // update potential
    potential = updatePotential(potential,currentDistance)

    for (arc <- currentMatching){
      if (!matching.contains(arc._2)){
        matching = matching ++ Map(arc)
      } else {
        matching = matching.removed(arc._2)
      }
    }

    // construct final matching
    val finalMatching = matching.map{
      case (site1,site2) if site2.id.contains("Right")=> (site1, searchByID(sitesLeft,site2.id.replace("Right","")))
      case (site1, site2) => (site1, site2)
    }
    /*
    println("final matching size here: ",matching.size)
    println("=============final matching=============")
    finalMatching.foreach{case(s1,s2) => println(s1.id,s2.id)}

     */

    // construct reduced cost matrix, c(i,j)' = c(i,j) + p(i) - p(j)
    val reducedCostMatrixAP : Map[Site, Map[Site, Double]] = costs.entries.map{
      case (site1,map1) => (site1, map1.map{
        case (site2, value) if branchNode.varAssignment(site1)(site2) == Some(true) => (site2,0)
        case (site2, value) if branchNode.varAssignment(site1)(site2) == Some(false) => (site2,inf)
        //case (site2, value) if potential(searchByID(sitesRight,site2.id+"Right")) == inf => (site2,inf)
        case (site2, value) if sitesLeft.contains(site1) && sitesRight.contains(searchByID(sitesRight,site2.id+"Right"))
        => (site2, costs.entries(site1)(site2)+ potential(site1)-
          potential(searchByID(sitesRight,site2.id+"Right")))
        case (site2, value) => (site2, value)
      })
    }

    // construct the result assignment data structure
    def constructResult(site1:Site ,site2:Site):Boolean= {
      if (finalMatching(site1)==site2) {
        //println(site1.id,site2.id)
        true}
      else {false}
    }

    val resultAssignment: arcWise[Boolean] = arcWise(branchNode.input, constructResult)
    //(resultAssignment.entries,costs.entries)
    (resultAssignment.entries,reducedCostMatrixAP)
  }

  // not used
  def computeLB (branchNode: BranchNode) : LowerBound = {
    0.0
  }
}
