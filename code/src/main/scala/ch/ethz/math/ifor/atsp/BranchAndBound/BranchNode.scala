package ch.ethz.math.ifor.atsp.BranchAndBound
import scala.util.control.Breaks._
import ch.ethz.math.ifor.atsp.{Input, Output, Site, Tour, arcWise, inf}
/** class for node classes like branch node
 * @param input contains input
 * @param varAssignment contains information for which variables are already set to 0 or 1
 */
class BranchNode(val input: Input,
                 var varAssignment: Map[Site, Map[Site, Option[Boolean]]],
                 val useAdditive:Boolean,
                 val isRootNode:Boolean,
                 val excludedArcAdded : Map[Site,Site],
                 val parentNode: BranchNode,
                 val useParametricAP:Boolean
                ) {
  var level = 0
  val costsMap: Map[Site, Map[Site, Double]] = input.distMat
  val lowerBoundAP: (Map[Site, Map[Site, IsLeafNode]], Map[Site, Map[Site, LowerBound]]) = {
    if (useParametricAP){
      if (parentNode == null){
        lowerBoundSolver.compute(branchNode = this)
      } else {
        paramatricSolver.compute(branchNode = this)
      }
    } else {
      lowerBoundSolver.compute(branchNode = this)
    }
  }

  var lowerBoundSolve: Map[Site, Map[Site, Boolean]] = lowerBoundAP._1
  var reducedCostMatrixAfterAP : Map[Site, Map[Site, Double]] = lowerBoundAP._2
  /*
  println("======reducedCostMatrixAfterAP in parent node=====")
  reducedCostMatrixAfterAP.collect{
    case (site1, map1)  =>  (site1, map1.collect{
      case (site2, value) => println(site1.id,site2.id,value)
    })
  }
  println("=======================================================")

   */
  var lowerBound: LowerBound  = lowerBoundSolve.map({case(site1, map1) => costsMap(site1)(map1.filter(_._2).head._1) }).sum
  val lowerBoundCostAP :Double = lowerBound

  //var reducedCostMatrix: Map[Site, Map[Site, Double]] = Map()
  //val naiveLowerBound: LowerBound = naiveLowerBoundSolver.computeLB(branchNode = this)

  val allTours: List[Tour] = detectToursV2(lowerBoundSolve)
  val isLeafNode: IsLeafNode = allTours.length == 1

  val globalHeuristic:(Double,Tour) = {
    if (isRootNode) {
      upperBoundSolver.computeUpperBound(this)
    } else {
      null
    }
  }

  val globalUpperbound:Double = {
    if (isRootNode) {
      //println("iniUB: ",globalHeuristic._1)
      globalHeuristic._1
    } else {
      inf
    }
  }

  var lowerBoundrSAP :Double= inf
  if (useAdditive /*&& isRootNode*/){
    if (lowerBoundCostAP != globalUpperbound){
      val inputRSAP = new Input(input.sites,reducedCostMatrixAfterAP)
      lowerBoundrSAP = rSAPLowerBoundSolver.compute(inputRSAP,this)
      lowerBound = lowerBound + lowerBoundrSAP
    }
  }

  def detectToursV2(lbSolve:Map[Site, Map[Site, Boolean]]):List[Tour] = {
    var pairMap = lbSolve.map({ case (site1, map1) => site1 -> map1.filter(_._2).head._1 })
    //println("====pairmap init=====")
    //pairMap.foreach{e => println(e._1,e._2)}
    var currentList : List[Site] =List()
    var listTours: List[Tour] = List()
    var currentArc = pairMap.head
    currentList = currentList :+ currentArc._1
    currentList = currentList :+ currentArc._2
    pairMap = pairMap - currentArc._1

    while (pairMap.nonEmpty){
      //println("====pairmap status=====")
      //pairMap.foreach{e => println(e._1,e._2)}

      if (pairMap.contains(currentList.last)){
        val nextArc = pairMap.find(_._1==currentList.last).get
        if (nextArc._2!=currentList.head){
          currentList = currentList :+ nextArc._2
          pairMap = pairMap - nextArc._1
          currentArc = nextArc
        } else {
          pairMap = pairMap - nextArc._1
          //println("=========")
          //currentList.foreach{e => println(e)}
          val findTour = new Tour(input,currentList)
          listTours = listTours :+ findTour
          currentList = currentList.drop(currentList.length)
          if (pairMap.nonEmpty){
            currentArc = pairMap.head
            currentList = currentList :+ currentArc._1
            currentList = currentList :+ currentArc._2
            pairMap = pairMap - currentArc._1
          }
        }
      } else {
        //println("=========")
        //currentList.foreach{e => println(e)}
        val findTour = new Tour(input,currentList)
        listTours = listTours :+ findTour
        currentList = currentList.drop(currentList.length)
        if (pairMap.nonEmpty){
          currentArc = pairMap.head
          currentList = currentList :+ currentArc._1
          currentList = currentList :+ currentArc._2
          pairMap = pairMap - currentArc._1
        }
      }
    }
    listTours

  }
/*

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
        //println("=========")
        //currentList.foreach{e => println(e)}
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
    /*
    var a = 0
    listTours.foreach{e => a= a+ e.sequence.length}
    println("a", a)

     */
    listTours
  }

 */

  // implement branchStep
  def branchStep: Either[BranchNode, List[BranchNode]] = {

    // either returns LeafNode at currentArc BranchNode,
    // or returns children of currentArc BranchNode

    // depending on what lowerBoundSolvers compute and output, adjust how LeafNodes are built

    if (isLeafNode) {//if lower bound is feasible tour, create leaf node
      println("find integer solution",allTours.length,allTours.head.sequence.length)
      Left(this)
    }
    else { //else use branching rule to get subproblems
      Right(branchingScheme.listChildren(branchNode = this)) //list of children
    }
  }
}
