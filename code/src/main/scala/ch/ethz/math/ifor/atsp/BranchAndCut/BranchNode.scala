package ch.ethz.math.ifor.atsp.BranchAndCut

import ch.ethz.math.ifor.atsp.{Input, Site, Tour, arcWise}
import com.google.ortools.linearsolver.{MPConstraint, MPObjective, MPSolver, MPVariable}

class BranchNode(val input: Input,
                 var varAssignment: Map[Site, Map[Site, Option[Boolean]]],
                 var solverLP:MPSolver,
                 var variables: arcWise[MPVariable],
                 var cut: List[MPConstraint]
                ) {
  var isRootNode: Boolean = false
  var parentNode: BranchNode = this

  // TODO: Question: Cannot get all constraints from solverLP directly? solver.constraints() doesn't work
  var solver:MPSolver = solverLP

  var cutsInNode: List[(Map[MPVariable,Double],Double)]= {
    if (isRootNode){
      List()
    } else {
      parentNode.cutsInNode
    }
  }
  var constraintsInNode: List[MPConstraint] = cut
  var variablesInNode: arcWise[MPVariable]= variables
  var reducedCosts:Map[Site, Map[Site, Double]]=Map()
  var level = 0
  val costsMap: Map[Site, Map[Site, Double]] = input.distMat
  def constructVariable(site1:Site ,site2:Site):MPVariable=
    if (site1.id==site2.id) {solverLP.makeNumVar(0,0,"")}
    else {
      //println("branchNode.varAssignment(site1)(site2)",branchNode.varAssignment(site1)(site2))
      varAssignment(site1)(site2) match {
        case null => solverLP.makeNumVar(0,1,"")
        case Some(true)=> solverLP.makeNumVar(1,1,"")
        case Some(false)=> solverLP.makeNumVar(0,0,"")
      }
    }

  val costs:arcWise[Double] = arcWise(input,input.distance)

  def fromCutToConstraint(cuts:List[(Map[MPVariable,Double],Double)]): Unit ={
    for (cut <- cuts){
      val constraint : MPConstraint = solverLP.makeConstraint(0, cut._2, "")
      cut._1.foreach{
        case (mpVariable,coefficient) => constraint.setCoefficient(mpVariable,coefficient)
      }
      constraintsInNode = constraintsInNode ++ List(constraint)
    }
  }

  var lowerBoundSolve: Map[Site, Map[Site, Double]] = linearProgrammingSolver.findSolution(input, variablesInNode, solverLP)

  val isInteger:Boolean = {
    var result = true
    lowerBoundSolve.collect{
      case (site1, map1) => (site1, map1.collect{
        case (site2, value) if (value != 0.0 && value != 1.0) => result = false
      })
    }
    result
  }

  //var reducedCostMatrix: Map[Site, Map[Site, Double]] = Map()
  //val naiveLowerBound: LowerBound = naiveLowerBoundSolver.computeLB(branchNode = this)
  val lowerBound: Double  = 0.0 // TODO

  def findTour(solution:Map[Site, Map[Site, Double]]):Tour={
    var pairMap = solution.map({ case (site1, map1) => site1 -> map1.filter(_._2 == 1.0).head._1 })
    var siteList : List[Site] = List()
    var currentSite = pairMap.head._1
    siteList = siteList ++ List(currentSite)
    while (pairMap.size > 1){
      val nextSite = pairMap(currentSite)
      siteList = siteList ++ List(nextSite)
      pairMap = pairMap.removed(currentSite)
      currentSite = nextSite
    }
    new Tour(input,siteList)
  }

  def detectTours(lbSolve:Map[Site, Map[Site, Double]]):List[Tour] = {

    var pairMap = lbSolve.map({ case (site1, map1) => site1 -> map1.filter(_._2==1.0).head._1 })

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

}
