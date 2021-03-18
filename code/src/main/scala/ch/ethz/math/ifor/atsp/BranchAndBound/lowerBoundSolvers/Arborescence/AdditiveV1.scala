package ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.Arborescence

import ch.ethz.math.ifor.atsp.BranchAndBound.{BranchNode, LowerBound}
import ch.ethz.math.ifor.atsp.{Input, Site, Tour, arcWise, inf}
import ch.ethz.math.ifor.atsp.BranchAndBound.lowerBoundSolvers.Arborescence.ChuLiuEdmondsV2


object AdditiveV1 {

  def compute(input: Input,branchNode: BranchNode):Double={

    var additiveLB : Double = 0.0

    val rSAPResult = ChuLiuEdmondsV2.compute(input, branchNode)
    additiveLB += rSAPResult._1

    /*
    val reducedCostMatrixAfterSAP = rSAPResult._2
    // transpose reduced cost matrix
    val transposedReducedCost = reducedCostMatrixAfterSAP.map{
      case (site1, map1) => (site1, map1.map{
        case (site2, value) => (site2, reducedCostMatrixAfterSAP(site2)(site1))
      })
    }

    val rSAAPResult = ChuLiuEdmondsV2.compute(new Input(input.sites,transposedReducedCost),branchNode)
    additiveLB += rSAAPResult._1

     */

    //rSAPResult._1
    additiveLB
  }






}
