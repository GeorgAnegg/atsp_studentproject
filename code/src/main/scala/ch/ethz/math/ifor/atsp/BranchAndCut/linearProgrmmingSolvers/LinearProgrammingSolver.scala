package ch.ethz.math.ifor.atsp.BranchAndCut.linearProgrmmingSolvers

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode
import ch.ethz.math.ifor.atsp.{Site, arcWise,Input}
import com.google.ortools.linearsolver.{MPConstraint, MPObjective, MPSolver, MPVariable}

trait LinearProgrammingSolver {
  def findSolution(input:Input,vars:arcWise[MPVariable],solver:MPSolver): Map[Site, Map[Site, Double]]

}
