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
    for (i <- branchNode.input.sites) {
      val constraint = solver.makeConstraint(1, 1, "")
      for (j <- branchNode.input.sites) {
        constraint.setCoefficient(x.search(i,j), 1)
      }
    }

    // Each site has at most one in-degree.
    for (j <- branchNode.input.sites) {
      val constraint = solver.makeConstraint(1, 1, "")
      for (i <- branchNode.input.sites) {
        constraint.setCoefficient(x.search(i,j), 1)
      }
    }

    // Create the objective function.
    val objective = solver.objective()
    for (i <- branchNode.input.sites) {
      for (j <- branchNode.input.sites) {
        objective.setCoefficient(x.search(i,j), costs.search(i,j))
      }
    }
    objective.setMinimization()

    //val resultStatus = solver.solve()

    def constructResult(site1:Site ,site2:Site):Boolean= {
      if (x.search(site1,site2).solutionValue == 1) {true}
      else {false}
    }

    val resultArray: arcWise[Boolean] = arcWise(branchNode.input, constructResult)

    resultArray.entries
    }

  // not used
  def computeLB (branchNode: BranchNode) : LowerBound = {
    0.0
  }
}