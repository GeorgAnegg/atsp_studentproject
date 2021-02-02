package ch.ethz.math.ifor.atsp.BranchAndCut

import ch.ethz.math.ifor.atsp.{Input, Site, Tour, arcWise}
import com.google.ortools.linearsolver.{MPConstraint, MPObjective, MPSolver, MPVariable}

class BranchNode(val input: Input,
                 var varAssignment: Map[Site, Map[Site, Option[Boolean]]]
                ) {
  var isRootNode: Boolean = false
  var parentNode: BranchNode = this

  var solverLP:MPSolver = {
    if (isRootNode){
      new MPSolver("LinearProgramming",
        MPSolver.OptimizationProblemType.GLOP_LINEAR_PROGRAMMING)
    }
    else{
      parentNode.solverLP
    }
  }

  var cuts: List[MPConstraint]= {
    if (isRootNode){
      List()
    } else {
      parentNode.cuts
    }
  }
  var variables: arcWise[MPVariable]= {
    if (isRootNode){
      arcWise(input, constructVariable)
    }
    else parentNode.variables
  }
  var reducedCosts:Map[Site, Map[Site, Double]]=Map()
  var level = 0
  val costsMap: Map[Site, Map[Site, Double]] = input.distMat
  var lowerBoundSolve: Map[Site, Map[Site, Double]] = linearProgrammingSolver.findSolution(input, variables, solverLP)

  val isInteger:Boolean = {
    var result = true
    lowerBoundSolve.foreach{
      case (site1, map1) => (site1, map1.foreach{
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
  // if root node, construct initial degree constraints
  if(isRootNode){
    //val variables: arcWise[MPVariable] = arcWise(input, constructVariable)

    var listConstraintsIn: List[MPConstraint] = List()
    var listConstraintsOut: List[MPConstraint] = List()

    // construct in- & out-degree constraints
    for (site1 <- input.sites){
      val constraintIn:MPConstraint = solverLP.makeConstraint(1, 1, "")
      val constraintOut:MPConstraint = solverLP.makeConstraint(1, 1, "")
      for (site2 <- input.sites){
        constraintIn.setCoefficient(variables.search(site1, site2),1)
        constraintOut.setCoefficient(variables.search(site2, site1),1)
      }
      listConstraintsIn = constraintIn::listConstraintsIn
      listConstraintsOut = constraintIn::listConstraintsOut
    }
    cuts = listConstraintsIn ++ listConstraintsOut
  }
  // construct the objective function.
  val objectiveFunction : MPObjective = solverLP.objective()
  variables.entries.map{
    case (site1, map1) => (site1, map1.map{
      case (site2, variable) => objectiveFunction.setCoefficient(variable,costs.search(site1, site2))
    })
  }



}
