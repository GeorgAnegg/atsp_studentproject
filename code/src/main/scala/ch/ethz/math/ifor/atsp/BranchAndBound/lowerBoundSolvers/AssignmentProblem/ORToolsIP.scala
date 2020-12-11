package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.AssignmentProblem

import ch.ethz.math.ifor.atsp.{Site, arcWise}
import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, LowerBound}
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.LowerBoundSolver
import com.google.ortools.linearsolver.MPConstraint
import com.google.ortools.linearsolver.MPObjective
import com.google.ortools.linearsolver.MPSolver
import com.google.ortools.linearsolver.MPVariable;

object ORToolsIP extends LowerBoundSolver{

  def compute(branchNode: BranchNode): Map[Site, Map[Site, Boolean]]  = {

    System.loadLibrary("jniortools")

    val numSites = branchNode.sitesStatus.size

    val solver: MPSolver = new MPSolver("AssignmentProblem",
      MPSolver.OptimizationProblemType.CBC_MIXED_INTEGER_PROGRAMMING)

    // x(i)(j) is an array of 0-1 variables, which will be 1
    // if site i is assigned to site j.
    //val x = Array.ofDim[MPVariable](numSites, numSites)


    def constructVariable(site1:Site ,site2:Site):MPVariable=
      if (site1==site2) {solver.makeIntVar(0,0,"")}
      else {
        branchNode.varAssignment(site1)(site2) match {
          case null => solver.makeBoolVar("")
          case Some(true)=> solver.makeIntVar(1,1,"")
          case Some(false)=> solver.makeIntVar(0,0,"")
        }
      }

    val x: arcWise[MPVariable] = arcWise[MPVariable](branchNode.input, constructVariable)

    //TODO: rewrite remaining section in terms of this x, maybe call them (active) variables or xs...

    val costs:arcWise[Double] = arcWise(branchNode.input,branchNode.input.distance)



//    for (i <- 0 until numSites) {
//      for (j <- 0 until numSites) {
//        if(i != j) {
//          //println("what happens",branchNode,branchNode.level,branchNode.sitesStatus(inputN.sites(i))(inputN.sites(j)))
//          x(i)(j) = if (branchNode.sitesStatus(branchNode.input.sites(i))(branchNode.input.sites(j)) == null) solver.makeIntVar(0, 1, "")
//          else if (branchNode.sitesStatus(branchNode.input.sites(i))(branchNode.input.sites(j)).contains(true)) solver.makeIntVar(1, 1, "")
//          else solver.makeIntVar(0, 0, "")
//        } else{
//          x(i)(j) = solver.makeIntVar(0, 0, "")
//        }
//        costs(i)(j) = branchNode.costsMap(branchNode.input.sites(i))(branchNode.input.sites(j))
//        //print(costs(i)(j)+"  ", i,j,inputN.sites(i),inputN.sites(j) )
//      }
//      //println("\r\n")
//    }

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
            println("Site " + branchNode.input.sites(i) + " assigned to Site " + branchNode.input.sites(j) + ".  Cost = " + costs(i)(j))
            resultArray(i)(j) = true
          } else {
            resultArray(i)(j) = false
          }
        }
      }
    }
    val resultBoolean : Map[Site, Map[Site, Boolean]] = branchNode.input.sites.zip(resultArray).map{case (site, distRow) =>
      site -> branchNode.input.sites.zip(distRow).toMap}.toMap
    resultBoolean
    }

  // not used
  def computeLB (branchNode: BranchNode) : LowerBound = {
    0.0
  }
}