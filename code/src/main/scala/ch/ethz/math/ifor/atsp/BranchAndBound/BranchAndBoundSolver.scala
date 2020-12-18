package ch.ethz.math.ifor.atsp.BranchAndBound

import ch.ethz.math.ifor.atsp.BranchAndBound.upperBoundSolver.Karp79.computeUpperBound
import ch.ethz.math.ifor.atsp.{Input, Output, Site, Solver}

import scala.util.control.Breaks.break

object BranchAndBoundSolver extends Solver {

  def solve(input: Input): Output = {

    // construct root node
    val numSites: Int = input.sites.length
    val initAssignmentArray: Array[Array[Option[Boolean]]] = Array.ofDim[Option[Boolean]](numSites, numSites)
    val initAssignmentMap: Map[Site, Map[Site, Option[Boolean]]] = input.sites.zip(initAssignmentArray).map{case (site, distRow) =>
      site -> input.sites.zip(distRow).toMap}.toMap
    val rootNode: BranchNode = new BranchNode(input, initAssignmentMap)
    rootNode.level = 0

    val upperBound = computeUpperBound(rootNode)
    val reducedThreshold = upperBound - rootNode.lowerBound

    var currentBestNode: Option[BranchNode] = None

    var activeBranches: List[BranchNode] = List(rootNode) // start with root node

    while (activeBranches.nonEmpty) {

      /** CT80 uses lowest-lower-bound search instead of depth-first search */
      val sortedNodes: List[BranchNode] = activeBranches.sortBy(_.lowerBound).reverse

      println("sorte nodes", "num active", sortedNodes.length)
      for (i <- sortedNodes) {
        println("activeb", i.lowerBound, i.level)
      }
      println("\r\n")

      val currentBranchNode = sortedNodes.head //consider node with smallest lower bound
      activeBranches = sortedNodes.reverse.init //remove considered node from active nodes

      println("active branches after sorted")
      for (i <- activeBranches) {
        println(i, i.lowerBound, i.level)
      }
      println("\r\n")
      println("current branchnode", currentBranchNode.level, "parent", currentBranchNode.parentNode.level)

      currentBranchNode.branchStep match {
        case Left(leaf) => // current node is leaf
          if (currentBestNode.isEmpty) {
            currentBestNode = Some(leaf)
            println("current best",currentBestNode.get.lowerBound)
            // println("length before",activeBranches.length)
            activeBranches = activeBranches.filter(_.lowerBound <= currentBestNode.get.lowerBound) // > should be <= ?
            // println("length after",activeBranches.length)
            // activeBranches = activeBranches.drop(activeBranches.length)
          } else if (leaf.lowerBound < currentBestNode.get.lowerBound) { //compare with current upper bound
            // println("compare", leaf.lowerBound, currentBestNode.get.lowerBound)
            currentBestNode = Some(leaf)
            activeBranches = activeBranches.filter(_.lowerBound <= currentBestNode.get.lowerBound) //prune remaining branches
          }
        case Right(children) => // current node gets branched
          println("Number of Children to be added" + children.size)
          for (child <- children) {
            if (currentBestNode.isEmpty) {
              println("add this children", child)
              activeBranches = activeBranches ++ List(child)
            } else if (child.lowerBound < currentBestNode.get.lowerBound) { //first check a naive lower bound for child node
              activeBranches = activeBranches ++ List(child) //add children/new branches
            }
          }
      }
    }

    val tour = currentBestNode.get.allTours.head
    new Output(input, tour)
  }
}
