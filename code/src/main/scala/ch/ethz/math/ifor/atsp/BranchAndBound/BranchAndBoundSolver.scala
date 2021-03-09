package ch.ethz.math.ifor.atsp.BranchAndBound

import ch.ethz.math.ifor.atsp.BranchAndBound.upperBoundSolvers
import ch.ethz.math.ifor.atsp.BranchAndBound.upperBoundSolvers.Patching.Karp79.computeUpperBound
import ch.ethz.math.ifor.atsp.{Input, Output, Site, Solver, inf}

import scala.util.control.Breaks.break

object BranchAndBoundSolver extends Solver {

  def solve(input: Input,formulation:String,preprocessing:Boolean,useAdditive:Boolean): Output = {
    // construct root node
    val numSites: Int = input.sites.length
    val initAssignmentArray: Array[Array[Option[Boolean]]] = Array.ofDim[Option[Boolean]](numSites, numSites)
    val initAssignmentMap: Map[Site, Map[Site, Option[Boolean]]] = input.sites.zip(initAssignmentArray).map{case (site, distRow) =>
      site -> input.sites.zip(distRow).toMap}.toMap
    val rootNode: BranchNode = new BranchNode(input, initAssignmentMap,useAdditive,true)
    rootNode.level = 0

    val iniHeuristic = rootNode.globalHeuristic
    val initUpperBound = iniHeuristic._1
    val initTour = iniHeuristic._2
    println("init upper bound",initUpperBound)

    var currentBestNode: Option[BranchNode] = None

    var activeBranches: List[BranchNode] = List(rootNode) // start with root node

    while (activeBranches.nonEmpty) {

      /** CT80 uses lowest-lower-bound search instead of depth-first search */
      //println("init upper bound",initUpperBound)

      if (activeBranches.minBy(_.lowerBound).lowerBound >= initUpperBound){
        println("here1?")
        return new Output(input, initTour)
      }

      val sortedNodes: List[BranchNode] = activeBranches.filter(_.lowerBound<=initUpperBound).sortBy(_.lowerBound)
      //println("Number of active sortedNodes", sortedNodes.length)

      /*
      println("num sortedNodes active", sortedNodes.length)
      for (i <- sortedNodes) {
        //println("activeb", i.lowerBound, i.level)
      }
      println("\r\n")

       */

      val currentBranchNode = sortedNodes.head //consider node with smallest lower bound
      activeBranches = sortedNodes.reverse.init //remove considered node from active nodes

      if (currentBranchNode.lowerBoundCostAP >= initUpperBound || currentBranchNode.lowerBound >= initUpperBound){
        println("here2?")
        return new Output(input, initTour)
      }
      //println("Number of active sortedNodes", sortedNodes.length,currentBranchNode.lowerBound,currentBranchNode.lowerBoundrSAP,currentBranchNode.lowerBoundCostAP,initUpperBound)

      /*
            println("active branches after sorted")
            for (i <- activeBranches) {
              println(i, i.lowerBound, i.level)
            }
            println("\r\n")
            println("current branchnode", currentBranchNode.level, "parent", currentBranchNode.parentNode.level)

       */

      currentBranchNode.branchStep match {
        case Left(leaf) => // current node is leaf
          if (currentBestNode.isEmpty) {
            println("here3?")
            currentBestNode = Some(leaf)
            //println("current best",currentBestNode.get.lowerBound)
            // println("length before",activeBranches.length)
            activeBranches = activeBranches.filter(_.lowerBound <= currentBestNode.get.lowerBound) // > should be <= ?
            // println("length after",activeBranches.length)
            // activeBranches = activeBranches.drop(activeBranches.length)
          } else if (leaf.lowerBound < currentBestNode.get.lowerBound) { //compare with current upper bound
            // println("compare", leaf.lowerBound, currentBestNode.get.lowerBound)
            println("here4?")
            currentBestNode = Some(leaf)
            activeBranches = activeBranches.filter(_.lowerBound <= currentBestNode.get.lowerBound) //prune remaining branches
          }
        case Right(children) => // current node gets branched
          //println("Number of Children to be added" + children.size)
          for (child <- children) {
            if (currentBestNode.isEmpty) {
              //println("add this children", child, "lb",child.lowerBound,child.lowerBoundCostAP,child.lowerBoundrSAP,child.globalUpperbound)
              activeBranches = activeBranches ++ List(child)
            } else if (child.lowerBound < currentBestNode.get.lowerBound) { //first check a naive lower bound for child node
              activeBranches = activeBranches ++ List(child) //add children/new branches
            }
          }
      }
    }

    val tour = currentBestNode.get.allTours.head
    val list = tour.listArcs
    list.foreach{e => println(e._1,e._2)}
    println("optimal length: ", tour.length,tour.sequence.length,input.sites.length)
    input.sites.foreach{e => println(e)}
    new Output(input, tour)
  }
}
