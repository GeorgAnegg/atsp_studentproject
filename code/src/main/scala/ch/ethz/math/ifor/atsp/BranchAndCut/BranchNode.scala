package ch.ethz.math.ifor.atsp.BranchAndCut

import ch.ethz.math.ifor.atsp.{Input, Site, Tour, arcWise, inf, negInf}
import com.google.ortools.linearsolver.{MPConstraint, MPObjective, MPSolver, MPVariable}

class BranchNode(val input: Input,
                 var varAssignment: Map[Site, Map[Site, Option[Boolean]]],
                 var globalConstraints: List[(Map[MPVariable,Double],Double)],
                 val formulation: String
                ) {
  var isRootNode: Boolean = false
  var parentNode: BranchNode = this
  var iteration: Int = 0

  System.loadLibrary("jniortools")

  val solverLP: MPSolver = new MPSolver("LinearProgramming",
    MPSolver.OptimizationProblemType.GLOP_LINEAR_PROGRAMMING)

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

  val variables: arcWise[MPVariable] = arcWise(input, constructVariable)
  val costs:arcWise[Double] = arcWise(input,input.distance)

  // construct in- & out-degree constraints
  for (site1 <- input.sites){
    val constraintIn:MPConstraint = solverLP.makeConstraint(1, 1, "")
    val constraintOut:MPConstraint = solverLP.makeConstraint(1, 1, "")
    for (site2 <- input.sites){
      constraintIn.setCoefficient(variables.search(site1, site2),1)
      constraintOut.setCoefficient(variables.search(site2, site1),1)
    }
  }

  if (formulation == "MTZ"){
    // construct order variables
    var orderVariables:Map[Site, MPVariable]=Map()
    input.sites.foreach {
      case site if (site!=input.sites.head)=> orderVariables += (site -> solverLP.makeNumVar(1, input.sites.length - 1, ""))
      case site if (site==input.sites.head)=> orderVariables += (site -> solverLP.makeNumVar(negInf, inf, ""))
    }
    // Add the order constraints
    for (i <- input.sites) {
      for (j <-input.sites){
        if (j != i && j!= input.sites.head){
          val constraint  = solverLP.makeConstraint(negInf,input.sites.length-2)
          constraint.setCoefficient(variables.search(i,j),input.sites.length-1)
          constraint.setCoefficient(orderVariables(i),1)
          constraint.setCoefficient(orderVariables(j),-1)
        }
      }
    }
  }

  // construct the objective function.
  val objectiveFunction : MPObjective = solverLP.objective()
  variables.entries.map{
    case (site1, map1) => (site1, map1.map{
      case (site2, variable) => objectiveFunction.setCoefficient(variable,costs.search(site1, site2))
    })
  }
  objectiveFunction.setMinimization()

  // TODO: Question: Cannot get all constraints from solverLP directly? solver.constraints() doesn't work
  var constraintsInNode: List[MPConstraint] = List()
  var reducedCosts:Map[Site, Map[Site, Double]]=Map()
  var level = 0
  val costsMap: Map[Site, Map[Site, Double]] = input.distMat

  def fromCutToConstraint(cuts:List[(Map[MPVariable,Double],Double)]): Unit ={
    for (cut <- cuts){
      val constraint : MPConstraint = solverLP.makeConstraint(negInf, cut._2, "")
      cut._1.foreach{
        case (mpVariable,coefficient) => constraint.setCoefficient(mpVariable,coefficient)
      }
      constraintsInNode = constraintsInNode ++ List(constraint)
    }
  }

  var lowerBoundSolve: Map[Site, Map[Site, Double]]= {
    if (globalConstraints.nonEmpty){
      for (cut<-globalConstraints){
        fromCutToConstraint(List(cut))
      }
    }
    linearProgrammingSolver.findSolution(input, variables, solverLP)
  }

  //var lowerBoundSolve: Map[Site, Map[Site, Double]] = linearProgrammingSolver.findSolution(input, variables, solverLP)

  var isInteger:Boolean = {
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
  def computeLowerBound(solution:Map[Site, Map[Site, Double]]):Double = {
    var result = 0.0
    solution.collect{
      case (site1, map1) => (site1, map1.collect{
        case (site2, value) if value!=0.0 => result += costsMap(site1)(site2)*value
      })
    }
    result
  }

  var lowerBound: Double = computeLowerBound(lowerBoundSolve)

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
    if(!isInteger){
      return List()
    }
    var pairMap = lbSolve.map({ case (site1, map1) => site1 -> map1.filter(_._2==1).head._1 })
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




}
