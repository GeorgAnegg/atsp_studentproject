package ch.ethz.math.ifor.atsp.BranchAndBound


import ch.ethz.math.ifor.atsp.{Input, Site, Tour}
import com.google.ortools.linearsolver.MPVariable

import scala.collection.mutable
/** class for node classes like branch node
 *
 * @param input
 * @param varAssignment contains information for which variables are already set to 0 or 1
 * @param
 */
class BranchNode(input: Input,
                 varAssignment: mutable.Map[Site, Map[Site, Option[Boolean]]]
                 ) {

  val inputNode: Input = input
  val costsMap: Map[Site, Map[Site, Double]] = input.distMat
  var sitesStatus: mutable.Map[Site, Map[Site, Option[Boolean]]] = varAssignment

  val lowerBoundSolve: Map[Site, Map[Site, Boolean]] = lowerBoundSolver.compute(branchNode = this)

  //TODO: compute this from the variable assignment in returned by lowerBoundSolve

  val lowerBound: LowerBound  = lowerBoundSolve.map({case(site1, map1) => costsMap(site1)(map1.filter(_._2).head._1) }).sum
  val allTours: List[Tour] = detectTours(lowerBoundSolve)
  val isLeafNode: IsLeafNode = allTours.length == 1

  def detectTours(lbSolve:Map[Site, Map[Site, Boolean]]):List[Tour] = {

    var pairMap = lbSolve.map({ case (site1, map1) => site1 -> map1.filter(_._2).head._1 })

    var listTours: List[Tour] = List()
    var currentList :List[Site] = List(pairMap.head._1, pairMap.head._2)

    pairMap = pairMap.drop(1)
    var currentArc = pairMap.head

    while (pairMap.nonEmpty) {

      var nextArc = pairMap.find(_._1.id == currentArc._2.id).get
      // if no tours created, keep tracking
      if (nextArc._2.id != currentList(1).id) {
        currentList  = currentList:::nextArc._1::Nil
        currentList  = currentList:::nextArc._2::Nil
        pairMap = pairMap.removed(currentArc._1)
        currentArc  = nextArc
      } else {
      // else, add the tour created, and staring tracking another remaining arc
        currentList  = currentList:::nextArc._1::Nil
        currentList  = currentList:::nextArc._2::Nil
        val findTour = new Tour(input,currentList)
        listTours = listTours:::findTour::Nil
        currentList = currentList.drop(currentList.length)
        pairMap = pairMap.removed(currentArc._1)
        currentArc = pairMap.head
        currentList  = currentList:::currentArc._1::Nil
        currentList  = currentList:::currentArc._2::Nil
      }
    }
    listTours
  }


  // TODO: implement branchStep
  def branchStep: Either[BranchNode, List[BranchNode]] = {

    // either returns LeafNode at currentArc BranchNode,
    // or returns children of currentArc BranchNode

    //TODO: depending on what lowerBoundSolvers compute and output, adjust how LeafNodes are built
    if (isLeafNode) { //if lower bound is feasible tour, create leaf node
      Left(this)
    }
    else { //else use branching rule to get subproblems
      Right(List()) //list of children
    }
  }
}
