package ch.ethz.math.ifor.atsp.BranchAndCut.linearProgrmmingSolvers

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode
import ch.ethz.math.ifor.atsp.{Site, arcWise}
import com.google.ortools.linearsolver.{MPConstraint, MPObjective, MPVariable}

trait LinearProgrammingSolver {
  def findSolution(variableLP:arcWise[MPVariable],constraintsLP:List[MPConstraint],objectiveLP:MPObjective): Map[Site, Map[Site, Double]]

}
