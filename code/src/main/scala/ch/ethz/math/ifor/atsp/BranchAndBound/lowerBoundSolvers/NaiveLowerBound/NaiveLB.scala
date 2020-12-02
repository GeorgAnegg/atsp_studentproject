package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.NaiveLowerBound
import ch.ethz.math.ifor.atsp.{Site,Input}
import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, IsLeafNode, LowerBound}
import com.google.ortools.linearsolver.{MPSolver, MPVariable}
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.{LowerBoundSolver, variables}

object NaiveLB extends LowerBoundSolver{
  def computeLB(branchNode: BranchNode): LowerBound  ={
    val resultLB: LowerBound

  }
}
