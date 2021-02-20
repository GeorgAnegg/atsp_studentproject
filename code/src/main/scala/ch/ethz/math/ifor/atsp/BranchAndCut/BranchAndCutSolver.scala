package ch.ethz.math.ifor.atsp.BranchAndCut
import ch.ethz.math.ifor.atsp.BranchAndCut.{BranchNode, upperBoundSolver}
import ch.ethz.math.ifor.atsp.{Input, Output, Site, Solver, Tour, arcWise}
import com.google.ortools.linearsolver.{MPConstraint, MPObjective, MPSolver, MPVariable}

object BranchAndCutSolver extends Solver {
  def solve(input: Input): Output = {

    // construct root node
    val numSites: Int = input.sites.length
    val initAssignmentArray: Array[Array[Option[Boolean]]] = Array.ofDim[Option[Boolean]](numSites, numSites)
    val initAssignmentMap: Map[Site, Map[Site, Option[Boolean]]] = input.sites.zip(initAssignmentArray).map { case (site, distRow) =>
      site -> input.sites.zip(distRow).toMap
    }.toMap

    System.loadLibrary("jniortools")

    val solverLP: MPSolver = new MPSolver("LinearProgramming",
      MPSolver.OptimizationProblemType.GLOP_LINEAR_PROGRAMMING)

    def constructVariable(site1:Site ,site2:Site):MPVariable= {
      if (site1.id==site2.id) {solverLP.makeNumVar(0,0,"")}
      else {
        solverLP.makeNumVar(0,1,"")
        }
    }

    val variables: arcWise[MPVariable] = arcWise(input, constructVariable)
    val costs:arcWise[Double] = arcWise(input,input.distance)
    var constraints: List[MPConstraint] = List()

    var listConstraintsIn: List[MPConstraint] = List()
    var listConstraintsOut: List[MPConstraint] = List()

    // construct in- & out-degree constraints
    for (site1 <- input.sites){
      val constraintIn:MPConstraint = solverLP.makeConstraint(1, 1, "")
      val constraintOut:MPConstraint = solverLP.makeConstraint(1, 1, "")
      for (site2 <- input.sites){
        constraintIn.setCoefficient(variables.search(site1, site2),1)
        constraintOut.setCoefficient(variables.search(site2, site1),1)
      }
      listConstraintsIn = constraintIn::listConstraintsIn
      listConstraintsOut = constraintIn::listConstraintsOut
    }
    constraints = constraints ++ listConstraintsIn ++ listConstraintsOut

    // construct the objective function.
    val objectiveFunction : MPObjective = solverLP.objective()
    variables.entries.map{
      case (site1, map1) => (site1, map1.map{
        case (site2, variable) => objectiveFunction.setCoefficient(variable,costs.search(site1, site2))
      })
    }

    val rootNode: BranchNode = new BranchNode(input,initAssignmentMap,solverLP,variables,constraints)
    rootNode.level = 0
    rootNode.isRootNode = true

    // Need a global cuts pool
    var globalCuts: List[(Map[MPVariable,Double],Double)] = List()
    // Add degree constraints to the global cuts pool

    var activeBranches: List[BranchNode] = List(rootNode) // start with root node
    var currentBestNode: Option[BranchNode] = None

    val initUpperBound = upperBoundSolver.computeUpperBound(rootNode)

    // TODO: Question, why in FT97, they didn't check if the solution is integer before applying separation algos?

    // solve LP associated
    // apply AP-pricing
    // remove some cuts, update core set
    // apply separation algorithm to find cuts tht cut off the current LP solution
    // 1. check all cuts in global pool
    // 2. if no cuts found, apply PR90 MINCUT algorithm for SEC separation
    // 3. if no cuts found, shrink, apply separation algorithms for comb, D_k and odd CAT
    // 4. if some cuts are found, add to the current LP and repeat, else if (...)
    while (activeBranches.nonEmpty) {

      val sortedNodes: List[BranchNode] = activeBranches.filter(_.lowerBound <= initUpperBound).sortBy(_.lowerBound)

      // println("num sortedNodes active", sortedNodes.length)

      var currentBranchNode = sortedNodes.head //consider node with smallest lower bound
      activeBranches = sortedNodes.reverse.init //remove considered node from active nodes

      print("Is integer? ",currentBranchNode.isInteger,"num of tours? ",currentBranchNode.detectTours(currentBranchNode.lowerBoundSolve).size)

      if (currentBranchNode.isInteger && currentBranchNode.detectTours(currentBranchNode.lowerBoundSolve).size==1){
        currentBestNode = Some(currentBranchNode)
        activeBranches = activeBranches.filter(_.lowerBound <= currentBestNode.get.lowerBound)
      }
      else {

        // apply AP-pricing
        //val solutionAfterPricing: Map[Site, Map[Site, Double]] = pricingScheme.updateColumns(currentBranchNode)

        //currentBranchNode.lowerBoundSolve = solutionAfterPricing

        val newCuts: List[(Map[MPVariable, Double], Double)] = cuttingPlane.findCuts(currentBranchNode, globalCuts)

        if (newCuts.nonEmpty) {
          // add cuts to current node and add to the branch list
          currentBranchNode.fromCutToConstraint(newCuts)
          globalCuts = globalCuts ++ newCuts
          activeBranches = activeBranches ++ List(currentBranchNode)

        } else {
          // check if current solution is integer
          if (currentBranchNode.isInteger) {
            // if integer, update current best solution
            currentBestNode = Some(currentBranchNode)
            activeBranches = activeBranches.filter(_.lowerBound <= currentBestNode.get.lowerBound)
          } else {
            // otherwise, branch and add to the branch list
            val children: List[BranchNode] = branchingScheme.listChildren(currentBranchNode)
            for (child <- children) {
              if (currentBestNode.isEmpty) {
                println("add this children", child)
                activeBranches = activeBranches ++ List(child)
              } else if (child.lowerBound < currentBestNode.get.lowerBound) { //first check a naive lower bound for child node
                activeBranches = activeBranches ++ List(child) //add children/new branches
              }
            }
          }
        }
      }

    }

    val tour = currentBestNode.get.detectTours(currentBestNode.get.lowerBoundSolve).head
    new Output(input, tour)

  }
}
