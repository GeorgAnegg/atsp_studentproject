package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.AssignmentProblem

import ch.ethz.math.ifor.atsp.Site
import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, LowerBound}
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.LowerBoundSolver
import com.google.ortools.linearsolver.MPConstraint;
import com.google.ortools.linearsolver.MPObjective;
import com.google.ortools.linearsolver.MPSolver;
import com.google.ortools.linearsolver.MPVariable;

object ORToolsIP extends LowerBoundSolver{

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
        if(i != j) {
          //println("what happens",branchNode,branchNode.level,branchNode.sitesStatus(inputN.sites(i))(inputN.sites(j)))
          x(i)(j) = if (branchNode.sitesStatus(inputN.sites(i))(inputN.sites(j)) == null) solver.makeIntVar(0, 1, "")
          else if (branchNode.sitesStatus(inputN.sites(i))(inputN.sites(j)).contains(true)) solver.makeIntVar(1, 1, "")
          else solver.makeIntVar(0, 0, "")
        } else{
          x(i)(j) = solver.makeIntVar(0, 0, "")
        }
        costs(i)(j) = branchNode.costsMap(inputN.sites(i))(inputN.sites(j))
        //print(costs(i)(j)+"  ", i,j,inputN.sites(i),inputN.sites(j) )
      }
      //println("\r\n")
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
    val objective = solver.objective()
    for (i <- 0 until numSites) {
      for (j <- 0 until numSites) {
        objective.setCoefficient(x(i)(j), costs(i)(j))
      }
    }
    objective.setMinimization()

    val resultStatus = solver.solve()
    val resultArray = Array.ofDim[Boolean](numSites, numSites)

    if (resultStatus == MPSolver.ResultStatus.OPTIMAL
      || resultStatus == MPSolver.ResultStatus.FEASIBLE) {
      println("Total cost: " + objective.value() + "\n")
      for (i <- 0 until numSites) {
        for (j <- 0 until numSites) {
          if (x(i)(j).solutionValue == 1) {
            println("Site " + inputN.sites(i) + " assigned to Site " + inputN.sites(j) + ".  Cost = " + costs(i)(j))
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

  // not used
  def computeLB (branchNode: BranchNode) : LowerBound = {
    0.0
  }
}