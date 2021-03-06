package ch.ethz.math.ifor.atsp.BranchAndCut.cuttingPlanes

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode
import com.google.ortools.linearsolver.MPVariable
import ch.ethz.math.ifor.atsp.BranchAndCut.cuttingPlanes.{MinCut,DkInequalities}

object SeparationStrategy extends CuttingPlane {
  def findCuts(branchNode:BranchNode,globalCuts:List[(Map[MPVariable,Double],Double)]):List[(Map[MPVariable,Double],Double)]={

    //List.concat(MinCut.findCuts(branchNode,globalCuts), DkInequalities.findCuts(branchNode,globalCuts))

    MinCut.findCuts(branchNode,globalCuts)
  }
}
