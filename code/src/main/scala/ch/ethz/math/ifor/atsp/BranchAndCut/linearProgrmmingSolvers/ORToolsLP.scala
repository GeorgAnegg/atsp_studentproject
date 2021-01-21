package ch.ethz.math.ifor.atsp.BranchAndCut.linearProgrmmingSolvers

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode
import ch.ethz.math.ifor.atsp.{Site, arcWise}
import com.google.ortools.linearsolver.{MPConstraint, MPObjective, MPSolver, MPVariable}

object ORToolsLP extends LinearProgrammingSolver {
  def findSolution(branchNode: BranchNode): Map[Site, Map[Site, Double]] = {

    val solver: MPSolver = new MPSolver("LinearProgramming",
      MPSolver.OptimizationProblemType.GLOP_LINEAR_PROGRAMMING)

    def constructVariable(site1:Site ,site2:Site):MPVariable=
      if (site1.id==site2.id) {solver.makeNumVar(0,0,"")}
      else {
        //println("branchNode.varAssignment(site1)(site2)",branchNode.varAssignment(site1)(site2))
        branchNode.varAssignment(site1)(site2) match {
          case null => solver.makeNumVar(0,1,"")
          case Some(true)=> solver.makeNumVar(1,1,"")
          case Some(false)=> solver.makeNumVar(0,0,"")
        }
      }

    val variables: arcWise[MPVariable] = arcWise(branchNode.input, constructVariable)

    val costs:arcWise[Double] = arcWise(branchNode.input,branchNode.input.distance)

    var listConstraintsIn: Map[Site, MPConstraint] = Map()
    var listConstraintsOut: Map[Site, MPConstraint] = Map()

    // construct in- & out-degree constraints
    for (site1 <- branchNode.input.sites){
      val constraintIn:MPConstraint = solver.makeConstraint(1, 1, "")
      val constraintOut:MPConstraint = solver.makeConstraint(1, 1, "")
      for (site2 <- branchNode.input.sites){
        constraintIn.setCoefficient(variables.search(site1, site2),1)
        constraintOut.setCoefficient(variables.search(site2, site1),1)
      }
      // record the site and the corresponding in- & out-constraints in order to obtain dual solutions later
      listConstraintsIn += (site1->constraintIn)
      listConstraintsOut += (site1->constraintOut)
    }

    // construct the objective function.
    val objectiveFunction : MPObjective = solver.objective()
    variables.entries.map{
      case (site1, map1) => (site1, map1.map{
        case (site2, variable) => objectiveFunction.setCoefficient(variable,costs.search(site1, site2))
      })
    }

    // set minimization problem
    objectiveFunction.minimization()


    def constructResult(site1:Site ,site2:Site):Double= {
      variables.search(site1,site2).solutionValue
    }
    val resultArray: arcWise[Double] = arcWise(branchNode.input, constructResult)
    resultArray.entries

  }


}
