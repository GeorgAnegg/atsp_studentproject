package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.AssignmentProblem

import ch.ethz.math.ifor.atsp.Site
import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, IsLeafNode, LowerBound}
import com.google.ortools.linearsolver.{MPSolver, MPVariable}
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.{LowerBoundSolver, variables}

object ORToolsIP extends  LowerBoundSolver{

  def compute(branchNode: BranchNode): Map[Site, Map[Site, Boolean]]  = {

    System.loadLibrary("jniortools")

    val numSites = branchNode.sitesStatus.size
    val inputN = branchNode.inputNode

    val solver: MPSolver = new MPSolver("AssignmentProblem",
      MPSolver.OptimizationProblemType.CBC_MIXED_INTEGER_PROGRAMMING)

    // x(i)(j) is an array of 0-1 variables, which will be 1
    // if site i is assigned to site j.
    val x = Array.ofDim[MPVariable](numSites, numSites)
    val costs = Array.ofDim[Double](numSites, numSites)

    for (i <- 0 until numSites) {
      for (j <- 0 until numSites) {
        x(i)(j) = if (branchNode.sitesStatus(inputN.sites(i))(inputN.sites(j)).isEmpty) solver.makeIntVar(0, 1, "")
        else if (branchNode.sitesStatus(inputN.sites(i))(inputN.sites(j)).contains(true)) solver.makeIntVar(1, 1, "")
        else solver.makeIntVar(0, 0, "")
        costs(i)(j) = branchNode.costsMap(inputN.sites(i))(inputN.sites(j))
      }
    }

    // Each site has at most one out-degree.
    for (i <- 0 until numSites) {
      val constraint = solver.makeConstraint(1, 1, "")
      for (j <- 0 until numSites) {
        constraint.setCoefficient(x(i)(j), 1)
      }
    }

    // Each site has at most one in-degree.
    for (j <- 0 until numSites) {
      val constraint = solver.makeConstraint(1, 1, "")
      for (i <- 0 until numSites) {
        constraint.setCoefficient(x(i)(j), 1)
      }
    }

    // Create the objective function.
    val objective = solver.objective
    for (i <- 0 until numSites) {
      for (j <- 0 until numSites) {
        objective.setCoefficient(x(i)(j), costs(i)(j))
      }
    }
    objective.setMinimization()

    val resultStatus = solver.solve
    var resultArray = Array.ofDim[Boolean](numSites, numSites)

    if (resultStatus == MPSolver.ResultStatus.OPTIMAL
      || resultStatus == MPSolver.ResultStatus.FEASIBLE) {
      println("Total cost: " + objective.value() + "\n")
      for (i <- 0 until numSites) {
        for (j <- 0 until numSites) {
          if (x(i)(j).solutionValue == 1) {
            println("Site " + i + " assigned to task " + j + ".  Cost = " + costs(i)(j))
            resultArray(i)(j) = true
          } else {
            resultArray(i)(j) = false
          }
        }
      }
    }
    val resultBoolean : Map[Site, Map[Site, Boolean]] = inputN.sites.zip(resultArray).map{case (site, distRow) =>
      site -> inputN.sites.zip(distRow).toMap}.toMap
    resultBoolean
    }
  }