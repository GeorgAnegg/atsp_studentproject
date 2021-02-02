package ch.ethz.math.ifor.atsp.BranchAndCut.linearProgrmmingSolvers

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode
import ch.ethz.math.ifor.atsp.{Site, arcWise,Input}
import com.google.ortools.linearsolver.{MPConstraint, MPObjective, MPSolver, MPVariable}

object ORToolsLP extends LinearProgrammingSolver {
  def findSolution(input:Input,vars:arcWise[MPVariable],solver:MPSolver): Map[Site, Map[Site, Double]] = {

    val result = solver.solve()

    // if (result == MPSolver.ResultStatus.OPTIMAL)

    def constructVariable(site1:Site ,site2:Site):Double={
      vars.search(site1, site2).solutionValue()
    }

    val resultSol:arcWise[Double] = arcWise(input,constructVariable)

    resultSol.entries

  }


}
