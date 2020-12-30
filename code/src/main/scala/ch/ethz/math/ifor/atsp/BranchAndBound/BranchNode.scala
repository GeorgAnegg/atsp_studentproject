package ch.ethz.math.ifor.atsp.BranchAndBound
import scala.util.control.Breaks._
import ch.ethz.math.ifor.atsp.{Input, Site, Tour,arcWise}
/** class for node classes like branch node
 * @param input contains input
 * @param varAssignment contains information for which variables are already set to 0 or 1
 */
class BranchNode(val input: Input,
                 var varAssignment: Map[Site, Map[Site, Option[Boolean]]]
                 ) {
  var level = 0
  val costsMap: Map[Site, Map[Site, Double]] = input.distMat
  val lowerBoundAP: (Map[Site, Map[Site, IsLeafNode]], Map[Site, Map[Site, LowerBound]]) =lowerBoundSolver.compute(branchNode = this)
  val lowerBoundSolve: Map[Site, Map[Site, Boolean]] = lowerBoundAP._1
  var reducedCostMatrixAfterAP : Map[Site, Map[Site, Double]] = lowerBoundAP._2
  val inputRSAP = new Input(input.sites,reducedCostMatrixAfterAP)
  val lowerBoundrSAP:Double = rSAPLowerBoundSolver.compute(inputRSAP)

  var parentNode: BranchNode = this
  //var reducedCostMatrix: Map[Site, Map[Site, Double]] = Map()
  //val naiveLowerBound: LowerBound = naiveLowerBoundSolver.computeLB(branchNode = this)
  val lowerBound: LowerBound  = lowerBoundSolve.map({case(site1, map1) => costsMap(site1)(map1.filter(_._2).head._1) }).sum

  val allTours: List[Tour] = detectTours(lowerBoundSolve)
  val isLeafNode: IsLeafNode = allTours.length == 1
  //println("tour length",allTours.length)
  //println("leafnode",isLeafNode)

  def detectTours(lbSolve:Map[Site, Map[Site, Boolean]]):List[Tour] = {

    var pairMap = lbSolve.map({ case (site1, map1) => site1 -> map1.filter(_._2).head._1 })

    var listTours: List[Tour] = List()
    var currentList :List[Site] = List(pairMap.head._1, pairMap.head._2)

    var currentArc = pairMap.head

    while (pairMap.size>1) {

      var nextArc = pairMap.find(_._1.id == currentArc._2.id).get
      // if no tours created, keep tracking
      if (nextArc._2.id != currentList.head.id) {
        // currentList  = currentList:::nextArc._1::Nil
        currentList  = currentList:::nextArc._2::Nil
        pairMap = pairMap.removed(currentArc._1)
        currentArc  = nextArc
      } else {
      // else, add the tour created, and staring tracking another remaining arc
        // currentList  = currentList:::nextArc._1::Nil
        // currentList  = currentList:::nextArc._2::Nil
        val findTour = new Tour(input,currentList)
        listTours = listTours:::findTour::Nil
        currentList = currentList.drop(currentList.length)
        pairMap = pairMap.removed(currentArc._1)
        pairMap = pairMap.removed(nextArc._1)
        if (pairMap.nonEmpty) {
          currentArc = pairMap.head
          currentList = currentList ::: currentArc._1 :: Nil
          currentList = currentList ::: currentArc._2 :: Nil
        }
      }
    }
    listTours
  }

  // implement branchStep
  def branchStep: Either[BranchNode, List[BranchNode]] = {

    // either returns LeafNode at currentArc BranchNode,
    // or returns children of currentArc BranchNode

    // depending on what lowerBoundSolvers compute and output, adjust how LeafNodes are built

    if (isLeafNode) { //if lower bound is feasible tour, create leaf node
      Left(this)
    }
    else { //else use branching rule to get subproblems
      Right(branchingScheme.listChildren(branchNode = this)) //list of children
    }
  }
}
