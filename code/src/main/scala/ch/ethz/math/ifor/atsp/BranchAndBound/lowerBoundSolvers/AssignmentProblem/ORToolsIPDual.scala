package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.AssignmentProblem

import ch.ethz.math.ifor.atsp.{Site, arcWise, inf}
import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, LowerBound}
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.LowerBoundSolver
import com.google.ortools.linearsolver.MPConstraint
import com.google.ortools.linearsolver.MPObjective
import com.google.ortools.linearsolver.MPSolver
import com.google.ortools.linearsolver.MPVariable

import scala.collection.mutable

object ORToolsIPDual extends LowerBoundSolver{
  def compute(branchNode: BranchNode): (Map[Site, Map[Site, Boolean]],Map[Site, Map[Site, Double]])  = {

    System.loadLibrary("jniortools")

    //assert(branchNode.varAssignment.keys.toVector == branchNode.input.sites,"distance matrix incomplete" )

    val solver: MPSolver = new MPSolver("AssignmentProblem",
      MPSolver.OptimizationProblemType.GLOP_LINEAR_PROGRAMMING)

    // x(i)(j) is an array of 0-1 variables, which will be 1
    // if site i is assigned to site j.
    //val x = Array.ofDim[MPVariable](numSites, numSites)

    var mapVarIn: Map[(Site,String),MPVariable]= Map()
    var mapVarOut: Map[(Site,String),MPVariable]= Map()

    for (site <- branchNode.input.sites){
      val variableIn : MPVariable = solver.makeNumVar(0,inf,"")
      val variableOut : MPVariable = solver.makeNumVar(0,inf,"")
      mapVarIn = mapVarIn ++ Map((site,"In")->variableIn)
      mapVarOut = mapVarOut ++ Map((site,"Out")->variableOut)
      }

    // u_i + v_j \leq c_{ij}
    for (site1 <- branchNode.input.sites){
      for (site2 <- branchNode.input.sites){
        if (branchNode.costsMap(site1)(site2)==Some(true)){
          val constraint:MPConstraint = solver.makeConstraint(0, 0, "")
          constraint.setCoefficient(mapVarIn((site1,"In")),1)
          constraint.setCoefficient(mapVarIn((site2,"Out")),1)
        } else if (branchNode.costsMap(site1)(site2)==Some(false)){
          // How to set up for excluded arcs?
          val constraint:MPConstraint = solver.makeConstraint(-inf, -1, "")
          constraint.setCoefficient(mapVarIn((site1,"In")),1)
          constraint.setCoefficient(mapVarIn((site2,"Out")),1)
        } else {
          val constraint: MPConstraint = solver.makeConstraint(-inf, branchNode.costsMap(site1)(site2), "")
          constraint.setCoefficient(mapVarIn((site1, "In")), 1)
          constraint.setCoefficient(mapVarIn((site2, "Out")), 1)
        }
      }
    }

    // Create the objective function.
    val objective = solver.objective()
    for (i <- mapVarIn) {
      objective.setCoefficient(i._2, 1)
    }
    for (i <- mapVarOut) {
      objective.setCoefficient(i._2, 1)
    }
    // println("num variables",solver.numVariables())
    objective.setMaximization()

    val resultStatus = solver.solve()
    println(resultStatus)

    if(resultStatus == MPSolver.ResultStatus.INFEASIBLE){
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

    //compute reduced cost
    val reducedCost = branchNode.costsMap.map{
      case (site1, map1) => (site1, map1.map{
        case (site2, value) => (site2, value - mapVarIn((site1,"In")).solutionValue() - mapVarOut((site2,"Out")).solutionValue())
      })
    }

    println("===============reduced cost===================== ")
    reducedCost.foreach{
      case (site1, map1) => map1.foreach{
        case (site2, value) => println(site1,site2,value)
      }
    }
    (Map(),reducedCost)
  }

  // not used
  def computeLB (branchNode: BranchNode) : LowerBound = {
    0.0
  }

}
