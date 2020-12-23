package ch.ethz.math.ifor.atsp

import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.AssignmentProblem.ORToolsIP
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.AssignmentProblem.HungarianAP
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.Arborescence.ChuLiuEdmonds

import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.NaiveLowerBound.NaiveLB
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.LowerBoundSolver
import ch.ethz.math.ifor.atsp.BranchAndBound.branchingSchemes.BranchingScheme
import ch.ethz.math.ifor.atsp.BranchAndBound.branchingSchemes.CT80


package object BranchAndBound {

  //val lowerBoundSolver: LowerBoundSolver = ORToolsIP
  val lowerBoundSolver: LowerBoundSolver = HungarianAP
  val naiveLowerBoundSolver: LowerBoundSolver = NaiveLB
  val rSAPLowerBoundSolver: LowerBoundSolver = ChuLiuEdmonds
  val branchingScheme: BranchingScheme = CT80
  type IsLeafNode = Boolean
  type LowerBound = Double

}
