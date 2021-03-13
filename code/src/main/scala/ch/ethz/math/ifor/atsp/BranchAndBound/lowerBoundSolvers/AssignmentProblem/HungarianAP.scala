package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.AssignmentProblem
import ch.ethz.math.ifor.atsp.{Site, arcWise, inf}
import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, LowerBound}
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.LowerBoundSolver

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, PriorityQueue}
import scala.util.control.Breaks.{break, breakable}
object HungarianAP extends LowerBoundSolver{

  def compute(branchNode: BranchNode): (Map[Site, Map[Site, Boolean]],Map[Site, Map[Site, Double]])  = {

    println("start hungarian ap")
    val numSites = branchNode.varAssignment.size

    /*
    val numSites = branchNode.varAssignment.size

    // construct matching map
    var matching : Map[Site,Site] = Map()
    var excluded : Map[Site,Site] = Map()

    for (map1 <- branchNode.varAssignment){
      for (map2 <- map1._2){
        if (map2._2 != null) {
          if (map2._2.get) {matching = matching + (map1._1 -> map2._1)}
          else {excluded= excluded + (map1._1 -> map2._1)}
        }
      }
    }

     */

    // construct cost map
    val costs:arcWise[Double] = arcWise(branchNode.input,branchNode.input.distance)
    /*
    println("=========== costs before AP ==========")
    costs.entries.foreach{
      case (site1, map1) => (site1,map1.foreach{
        case(site2, value) => println(site1,site2,value)
      })
    }

     */
    var matching : Map[Site,Site] = Map()
    var stMap : Map[Site,Map[Site,Double]] = costs.entries.map {
      case (site1, map1) => (site1, map1.map {
        //case (site2, value) if value > 99999 => (new Site(site2.id + "Right"), 99999)
        case (site2, value) => (new Site(site2.id + "Right"), value)
      })
    }

    // construct left sites set
    val sitesLeft : Map[Site,String] = stMap.keys.map{e => (e->e.id)}.toMap
    var sitesRight : Map[Site,String] = Map()
    stMap.foreach{
      case (site1, map1) => map1.foreach{
        case (site2, value) => sitesRight = sitesRight ++ Map(site2->site2.id)
      }
    }

    /*

    println("--------------------------costs=======================")

    costs.entries.map{
      case (site1, map1) => (site1, map1.map{
        case (site2, value)=> println(site1.id,site2.id,value)
      } )
    }
    println("--------------------------=======================")

    val mapV1 = costs.entries.collect {
      case (site1, map1) => (site1, map1.collect {
        case (site2, value) if site1!=site2 && branchNode.varAssignment(site1)(site2)==null && !matching.exists(_._1==site1) &&
          !matching.exists(_._2==site2) => (new Site(site2.id+"Right"),value)
      })
    }

     */
    //val mapV1 = costs.entries

    // create s, t nodes
    val start : Site = new Site("s")
    val destination : Site = new Site("t")

    // implement a function to search a site by id among all sites
    def searchByID(sites:Map[Site,String],identity:String):Site={
      sites.find(_._1.id==identity).get._1
      /*
      var result: Site = null
      if (sites.values.toList.contains(identity)){
        result = sites.find(_._2==identity).get._1
      }
      result

       */
    }

    /*
    val costPrime = costs.entries.map {
      case (site1, map1) => (site1, map1.map {
        case (site2, value) if matching.keys.exists(_==site1) || matching.values.exists(_ == site2) => (new Site(site2.id+"Right"), inf)
        case (site2, value) if !matching.keys.exists(_==site1) && !matching.values.exists(_ == site2) => (new Site(site2.id+"Right"), value)
      })
    }

    /*
    var sitesRight : Map[Site,String] = sites.filterNot(matching.values.toList.contains(_))
    sitesRight = sitesRight.map{
      case (site,str) => (new Site(str+"Right"),str+"Right")
    }

     */

    // construct arc between left sites set and right sites set
    // TODO: Seems slow!
    println("start construct residual map")
    val mapV1 = costPrime.collect{
      case(site1,map1)=>(site1,map1.collect{
        case(site2,_) if branchNode.varAssignment(site1)(site2) == Some(false) || site1 == site2 => (sitesRight.find(_._2==site2.id+"Right").get._1,inf)
        case(site2,value) => (sitesRight.find(_._2==site2.id+"Right").get._1,value)
      })}
    println("end construct residual map")

     */


    // construct right sites set

    // add arcs from s to nodes in left sites set
    //var stMap :Map[Site,Map[Site, Double]]= mapV1
    sitesLeft.foreach(site => stMap = stMap ++ Map(start -> Map(site._1 -> 0.0)))

    // add arcs from nodes in right sites set to t
    sitesRight.foreach{e=>
      stMap = stMap + (e._1 -> Map(destination->0))
    }
    /*

    println("--------------------------stMap=======================")

    stMap.map{
      case (site1, map1) => (site1, map1.map{
        case (site2, value)=> println(site1.id,site2.id,value)
      } )
    }
    println("--------------------------=======================")

     */

    /*
    println("=========== stMap ==========")
    stMap.foreach{
      case (site1, map1) => (site1,map1.foreach{
        case(site2, value) => println(site1.id,site2.id,value)
      })
    }

     */

    // construct array for all sites, i.e., s + left + right + t
    val allSites:Map[Site,String] = sitesLeft++ sitesRight ++ Map(start->start.id) ++ Map(destination->destination.id)
    /*
    allSites.foreach{e=>println("all sites",e,e.id)}

     */

    // construct potential map
    var potential : Map[Site,Double] = {
      allSites.map{
         site => site._1->0.0
      }.toMap
    }

    // implement function to update potential
    def updatePotential(potential:Map[Site,Double],dijkstraDistance:Map[Site,Double]): Map[Site,Double]={
      potential.map{case (site,value) => (site,dijkstraDistance(site)+value)}
    }

    // implement function to update residual graph
    def updateResidualGraph(graph: Map[Site,Map[Site,Double]], arcs :Map[Site,Site], currentDistance:Map[Site,Double]): Map[Site,Map[Site,Double]] = {

      val result :Map[Site, Map[Site, Double]]= graph.collect{
        case (site1,map1) if site1.id != "s" && site1.id != "t" && arcs.contains(site1)
        => (site1,map1+(arcs(site1)->0.0)-arcs.find(_._2.id == site1.id).get._1)
        case (site1,map1) if site1.id == "s" => (site1,map1-arcs.find(_._2.id == site1.id).get._1)
        case (site1,map1) if site1.id == "t" =>(site1,map1+(arcs(site1)->0.0))
        case (site1,map1) => (site1,map1)
      }

      val result2: Map[Site, Map[Site, Double]]= result.map{ case(site1,map1) => (site1,map1.map{
        case (site2,value) if arcs.exists(x => x._1 == site1 && x._2 == site2) || arcs.exists(x => x._1 == site2 && x._2 == site1) => (site2,value)
        case (site2,value) if !arcs.exists(x => x._1 == site1 && x._2 == site2) && !arcs.exists(x => x._1 == site2 && x._2 == site1)
          && site2.id != "t"=> (site2,currentDistance(site1)+value-currentDistance(site2))
        case (site2,_) if !arcs.exists(x => x._1 == site1 && x._2 == site2) && !arcs.exists(x => x._1 == site2 && x._2 == site1) && site2.id == "t"=> (site2,0)
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
          case (site, _) => site._1 -> inf
        }.to(collection.mutable.Map)
      }

      // set the distance of node s to 0.0
      dijkstraDist.update(start, 0.0)

      // construct the queue, and the labeling array
      var explored: Array[Site] = Array()
      var queue: List[(Site, Double)] = List((start, 0.0))

      // while all nodes are not visited, continue the process

      while (queue.nonEmpty && explored.length != allSites.size-1) {

        // take the first element, i.e., lowest-weighted unvisited node
        queue = queue.sortBy(_._2)
        val currentSite = queue.head
        breakable {
          if (currentSite._1.id == "t") break
        }
        /*
        println("current site", currentSite._1, currentSite._1.id, currentSite._2)
        println("queue")
        for (i <- queue) {
          println(i._1, i._1.id, i._2)
        }

         */

        // delete the current node from the queue and label it as visited
        queue = queue.drop(1)

        explored = explored :+ currentSite._1

        // construct all the sites that can be reached from currentSite
        val reachable = graph(currentSite._1).keys
        /*
                  println("reachable")
                  for (i <- reachable) {
                    println(i.id, i)
                  }

         */

        // if min-distance can be updated, update; add all these sites to the queue
        reachable.foreach { nextsite =>
          if (dijkstraDist(nextsite) > dijkstraDist(currentSite._1) + graph(currentSite._1)(nextsite)) {
            dijkstraDist.update(nextsite, dijkstraDist(currentSite._1) + graph(currentSite._1)(nextsite))
            dijkstraPre.update(nextsite, currentSite._1)
            //println("add", (nextsite, nextsite.id, dijkstraDist(nextsite)))
            if (queue.exists(_._1 == nextsite)){
              queue = queue.filter(_._1!=nextsite)
            }
            if (nextsite.id!="t") {
              queue = queue ::: (nextsite, dijkstraDist(nextsite)) :: Nil
            }
          }
        }

        println("explored",explored.length,allSites.size,queue.nonEmpty)
        for (i<-explored){
          println(i,i.id+"  ")
        }
        println("dijkstraDist")
        dijkstraDist.foreach {
          e => println(e._1, e._1.id, e._2)
        }


      }

      // construct a t-s path from dijkstraPre
      var path: Array[Site] = Array(destination)
      while (path.last.id != "s") {
        path = path :+ dijkstraPre(path.last)
      }

      //path.foreach{e=>println(e,e.id)}

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
      /*
      print("newMatching matching")
      newMatching.foreach{e=>println(e._1,e._1.id,e._2,e._2.id)}

       */

      // return a distance map and a path map
      (dijkstraDist.toMap, arcs, newMatching)
    }


    println("matching size: "+matching.size)
    /*
    println("--------------------------stMap=======================")

    stMap.map{
      case (site1, map1) => (site1, map1.map{
        case (site2, value)=> println(site1,site2,value)
      } )
    }
    println("--------------------------=======================")

     */
    // while the size of matching is not equal to numSite, iteratively call dijkstra after updating parameters
    while(matching.size != numSites) {

      val currentResult = dijkstra(stMap)
      val currentDistance = currentResult._1
      val currentPath = currentResult._2
      val currentMatching = currentResult._3
      // update potential

      // if current AP is infeasible
      if (currentPath.isEmpty){
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

      potential = updatePotential(potential,currentDistance)

      // update residual map
      stMap = updateResidualGraph(stMap, currentPath, currentDistance)

      // update matching
      matching = updateMatching(currentMatching, matching)
      /*

      println("=============currentDistance=============")
      currentDistance.foreach{e => println(e._1.id,e._2)}
      println("=============potential=============")
      potential.foreach{e => println(e._1.id,e._2)}
      println("=============path=============")
      currentPath.foreach{e => println(e._1.id,e._2.id)}
      println("=============matching=============")
      matching.foreach{case(s1,s2) => println(s1.id,s2.id)}

       */
      println("size of matching now: ",matching.size)
    }

/*
    println("=============potential=============")
    potential.foreach{e => println(e._1.id,e._2)}

 */

    // construct reduced cost matrix, c(i,j)' = c(i,j) + p(i) - p(j)
    /*
    val reducedCostMatrixAP : Map[Site, Map[Site, Double]] = costs.entries.map{
      case (site1,map1) => (site1, map1.map{
        case (site2, _) if branchNode.varAssignment(site1)(site2) == Some(true) => (site2,0)
        case (site2, _) if branchNode.varAssignment(site1)(site2) == Some(false) => (site2,inf)
        case (site2, _) if potential(searchByID(sitesRight,site2.id+"Right")) == inf => (site2,inf)
        case (site2, _) => (site2, mapV1(site1)(searchByID(sitesRight,site2.id+"Right"))+
          potential(site1)-potential(searchByID(sitesRight,site2.id+"Right")))
      })
    }

     */

    println("--------------------------stMap=======================")

    stMap.collect{
      case (site1, map1) => (site1, map1.collect{
        case (site2, value) => println(site1.id,site2.id,value)
      } )
    }
    println("--------------------------=======================")


    // construct final matching
    val finalMatching = matching.map{
      case (site1,site2) => (site1, searchByID(sitesLeft,site2.id.replace("Right","")))
    }
    /*
    println("===========final matching=======")
    finalMatching.foreach{
      case (site1,site2) => println(site1.id,site2.id,branchNode.costsMap(site1)(site2))
    }

     */

    // construct the result assignment data structure
    def constructResult(site1:Site ,site2:Site):Boolean= {
      if (finalMatching(site1)==site2) {
        //println(site1,site2,reducedCostMatrixAP(site1)(site2))
        true}
      else {false}
    }

    val resultAssignment: arcWise[Boolean] = arcWise(branchNode.input, constructResult)

    // 1. set p,residual cost
    // 2. apply dij
    // 3. find shortest s-t path, add to Matching M
    // 4. update p and residual cost
    // 5. apply dij until |M| = n

    // dij takes a residual graph, returns d(i) for all i

    //(resultAssignment.entries,costs.entries)
    (resultAssignment.entries,stMap)
  }

  // not used
  def computeLB (branchNode: BranchNode) : LowerBound = {
    0.0
  }

}
