package ch.ethz.math.ifor.atsp.BranchAndCut.branchingSchemes

import ch.ethz.math.ifor.atsp.BranchAndCut.BranchNode
import ch.ethz.math.ifor.atsp.Site

object naiveBranching extends BranchingSchemes {

  def min(d: Double, d1: Double):Double={
    if (d<=d1){
      d
    } else {
      d1
    }
  }

  def listChildren(branchNode: BranchNode): List[BranchNode] = {
    def scoreVariable(site1:Site,site2:Site):Double={
      val valueLP = branchNode.lowerBoundSolve(site1)(site2)
      val cost = branchNode.input.distMat(site1)(site2)
      cost *  min(valueLP,1-valueLP)
    }

    var currentMax = 0.0

    var currentBestVariable : Map[Site,Site] = Map()

    for (site1 <- branchNode.input.sites){
      for (site2 <- branchNode.input.sites){
        val thisScore = scoreVariable(site1,site2)
        if (thisScore > currentMax){
          currentBestVariable = Map(site1->site2)
          currentMax = thisScore
        }
      }
    }

    val childMapLeft:Map[Site,Map[Site, Option[Boolean]]] = branchNode.varAssignment.map{
      case (site1, map1) => (site1,map1.map{
        case (site2, value) if (site1 == currentBestVariable.head._1 && site2 == currentBestVariable.head._2) => (site2, Some(true))
        case (site2, value) if (site1 != currentBestVariable.head._1 || site2 != currentBestVariable.head._2) => (site2, value)
      })
    }

    val childMapRight:Map[Site,Map[Site, Option[Boolean]]] = branchNode.varAssignment.map{
      case (site1, map1) => (site1,map1.map{
        case (site2, value) if (site1 == currentBestVariable.head._1 && site2 == currentBestVariable.head._2) => (site2, Some(false))
        case (site2, value) if (site1 != currentBestVariable.head._1 || site2 != currentBestVariable.head._2) => (site2, value)
      })
    }

    val childLeft = new BranchNode(branchNode.input, childMapLeft)
    val childRight = new BranchNode(branchNode.input, childMapRight)

    childLeft.parentNode = branchNode
    childLeft.level = branchNode.level + 1
    childRight.parentNode = branchNode
    childRight.level = branchNode.level + 1

    List(childLeft,childRight)

    // TODO: Add Heuristic rule: prefer variables between 0.4 and 0.8
  }

}
