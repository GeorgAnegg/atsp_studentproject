package ch.ethz.math.ifor.atsp.BranchAndBound
import ch.ethz.math.ifor.atsp.{Input, Output, Site, Solver}

object BranchAndCutSolver extends Solver{
  def solve (input: Input): Output ={

    // construct root node
    val numSites: Int = input.sites.length
    val initAssignmentArray: Array[Array[Option[Boolean]]] = Array.ofDim[Option[Boolean]](numSites, numSites)
    val initAssignmentMap: Map[Site, Map[Site, Option[Boolean]]] = input.sites.zip(initAssignmentArray).map{case (site, distRow) =>
      site -> input.sites.zip(distRow).toMap}.toMap
    val rootNode: BranchNode = new BranchNode(input, initAssignmentMap)
    rootNode.level = 0

    val test =linearProgrammingSolver.findSolution(rootNode)

    val sol = rootNode.allTours
    new Output(input, sol.head)

  }

}
