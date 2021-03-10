package ch.ethz.math.ifor.atsp.BranchAndCut
import ch.ethz.math.ifor.atsp.{Input, Output, Site, Solver, Tour, arcWise, inf}
import com.google.ortools.linearsolver.{MPConstraint, MPObjective, MPSolver, MPVariable}

object BranchAndCutSolver extends Solver {
  def solve(input: Input, formulation:String, preprocessing:Boolean,useAddditive:Boolean,useParametricAP:Boolean): Output = {
    System.loadLibrary("jniortools")

    // construct root node
    val numSites: Int = input.sites.length
    val initAssignmentArray: Array[Array[Option[Boolean]]] = Array.ofDim[Option[Boolean]](numSites, numSites)
    val initAssignmentMap: Map[Site, Map[Site, Option[Boolean]]] = input.sites.zip(initAssignmentArray).map { case (site, distRow) =>
      site -> input.sites.zip(distRow).toMap
    }.toMap

    var initUpperBound = inf
    var initTour:Tour = new Tour(input,input.sites.toList)

    if (preprocessing){
      // solve initial AP to get an upperbound and do reduction
      val solverLP: MPSolver = new MPSolver("AssignmentProblem",
        MPSolver.OptimizationProblemType.GLOP_LINEAR_PROGRAMMING)
      def constructVariable(site1:Site ,site2:Site):MPVariable= {
        if (site1.id==site2.id) {solverLP.makeIntVar(0,0,"")}
        else {
          solverLP.makeIntVar(0,1,"")
        }
      }
      val variables: arcWise[MPVariable] = arcWise(input, constructVariable)
      val costs:arcWise[Double] = arcWise(input,input.distance)
      // construct in- & out-degree constraints
      for (site1 <- input.sites){
        val constraintIn:MPConstraint = solverLP.makeConstraint(1, 1, "")
        val constraintOut:MPConstraint = solverLP.makeConstraint(1, 1, "")
        for (site2 <- input.sites){
          constraintIn.setCoefficient(variables.search(site1, site2),1)
          constraintOut.setCoefficient(variables.search(site2, site1),1)
        }
      }
      // construct the objective function.
      val objectiveFunction : MPObjective = solverLP.objective()
      variables.entries.map{
        case (site1, map1) => (site1, map1.map{
          case (site2, variable) => objectiveFunction.setCoefficient(variable,costs.search(site1, site2))
        })
      }
      objectiveFunction.setMinimization()
      val resultStatus = solverLP.solve()
      val apAssignment = initAssignmentMap.map{
        case (site1, map1) => (site1, map1.map{
          case (site2, value) if variables.search(site1,site2).solutionValue == 1.0 => (site2, true)
          case (site2, value) => (site2, false)
        })
      }
      def detectTours(lbSolve:Map[Site, Map[Site, Boolean]]):List[Tour] = {
        var pairMap = lbSolve.map({ case (site1, map1) => site1 -> map1.filter(_._2).head._1 })
        var currentList : List[Site] =List()
        var listTours: List[Tour] = List()
        var currentArc = pairMap.head
        currentList = currentList :+ currentArc._1
        currentList = currentList :+ currentArc._2
        pairMap = pairMap - currentArc._1

        while (pairMap.nonEmpty){
          if (pairMap.contains(currentList.last)){
            val nextArc = pairMap.find(_._1==currentList.last).get
            if (nextArc._2!=currentList.head){
              currentList = currentList :+ nextArc._2
              pairMap = pairMap - nextArc._1
              currentArc = nextArc
            } else {
              pairMap = pairMap - nextArc._1
              val findTour = new Tour(input,currentList)
              listTours = listTours :+ findTour
              currentList = currentList.drop(currentList.length)
              if (pairMap.nonEmpty){
                currentArc = pairMap.head
                currentList = currentList :+ currentArc._1
                currentList = currentList :+ currentArc._2
                pairMap = pairMap - currentArc._1
              }
            }
          } else {
            val findTour = new Tour(input,currentList)
            listTours = listTours :+ findTour
            currentList = currentList.drop(currentList.length)
            if (pairMap.nonEmpty){
              currentArc = pairMap.head
              currentList = currentList :+ currentArc._1
              currentList = currentList :+ currentArc._2
              pairMap = pairMap - currentArc._1
            }
          }
        }
        listTours
      }
      val apTours = detectTours(apAssignment)
      val apCost: Double  = apAssignment.map({case(site1, map1) => input.distMat(site1)(map1.filter(_._2).head._1) }).sum
      val iniHeuristic = upperBoundSolver.computeUpperBound(apCost,apTours,input)
      initUpperBound = iniHeuristic._1
      initTour = iniHeuristic._2
      if (apCost == initUpperBound){
        return new Output(input, initTour)
      }
      println("ini upper: ", +initUpperBound)
    }

    // Need a global cuts pool
    var globalCuts: List[(Map[MPVariable,Double],Double)] = List()

    val rootNode: BranchNode = new BranchNode(input,initAssignmentMap,globalCuts,formulation)
    rootNode.level = 0
    rootNode.isRootNode = true

    var activeBranches: List[BranchNode] = List(rootNode) // start with root node
    var currentBestNode: Option[BranchNode] = None

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
      //println("Number of active nodes: "+activeBranches.size)

      val sortedNodes: List[BranchNode] = activeBranches.filter(_.lowerBound <= initUpperBound).sortBy(_.lowerBound)

      // println("num sortedNodes active", sortedNodes.length)

      var currentBranchNode = sortedNodes.head //consider node with smallest lower bound
      activeBranches = sortedNodes.reverse.init //remove considered node from active nodes

      if (currentBranchNode.lowerBound == initUpperBound){
        return new Output(input, initTour)
      }

      //print("Is integer? ",currentBranchNode.isInteger,"num of tours? ",currentBranchNode.detectTours(currentBranchNode.lowerBoundSolve).size)

      if (currentBranchNode.isInteger && currentBranchNode.detectTours(currentBranchNode.lowerBoundSolve).size==1){
        currentBestNode = Some(currentBranchNode)
        activeBranches = activeBranches.filter(_.lowerBound <= currentBestNode.get.lowerBound)
        //println("Integer solution with one tour, num of cuts inside: ",currentBranchNode.globalConstraints.size)

      } else if (currentBranchNode.isInteger){
        // heuristic, if the solution is integer and contains some subtours, add corresponding DFJ SECs
        for (subtour <- currentBranchNode.detectTours(currentBranchNode.lowerBoundSolve)){
          var resultMap: Map[MPVariable, Double] = Map()
          val set1 = subtour.sequence
          val set2 = input.sites.toList diff set1
          for (node1 <- set1) {
            for (node2 <- set1) {
              resultMap = resultMap ++ Map(currentBranchNode.variables.search(node1, node2) -> 1.0)
              resultMap = resultMap ++ Map(currentBranchNode.variables.search(node2, node1) -> 1.0)
            }
          }
          val newcut:List[(Map[MPVariable, Double], Double)] =  List((resultMap, set1.size-1))
          globalCuts = globalCuts ++ newcut
          currentBranchNode.fromCutToConstraint(newcut)
          currentBranchNode.globalConstraints = globalCuts
        }
        currentBranchNode.lowerBoundSolve = linearProgrammingSolver.findSolution(input, currentBranchNode.variables, currentBranchNode.solverLP)
        currentBranchNode.isInteger = {
          var result = true
          currentBranchNode.lowerBoundSolve.collect{
            case (site1, map1) => (site1, map1.collect{
              case (site2, value) if (value != 0.0 && value != 1.0) => result = false
            })
          }
          result
        }
        currentBranchNode.lowerBound = currentBranchNode.computeLowerBound(currentBranchNode.lowerBoundSolve)
        activeBranches = activeBranches ++ List(currentBranchNode)
        //println("Integer solution with more than one subtours, num of cuts inside: ",currentBranchNode.globalConstraints.size)
      }
      else {
        // apply AP-pricing
        // set threshold to 2000 temporarily because it largely slows down the computational time for current instances
        if (numSites >= 2000) {
          currentBranchNode = pricingScheme.updateColumns(currentBranchNode)
        }
        // val solutionAfterPricing: Map[Site, Map[Site, Double]] = pricingScheme.updateColumns(currentBranchNode)

        // currentBranchNode.lowerBoundSolve = solutionAfterPricing

        val newCuts: List[(Map[MPVariable, Double], Double)] = cuttingPlane.findCuts(currentBranchNode, globalCuts)

        // TODO: check the slack of the cuts, if > 0.01 and number of cuts > 10, remove the cut
        if (newCuts.nonEmpty && currentBranchNode.iteration <= 5) {

          // add cuts to current node and add to the branch list
          globalCuts = globalCuts ++ newCuts
          currentBranchNode.fromCutToConstraint(newCuts)
          currentBranchNode.globalConstraints = globalCuts
          currentBranchNode.lowerBoundSolve = linearProgrammingSolver.findSolution(input, currentBranchNode.variables, currentBranchNode.solverLP)
          currentBranchNode.isInteger = {
            var result = true
            currentBranchNode.lowerBoundSolve.collect{
              case (site1, map1) => (site1, map1.collect{
                case (site2, value) if (value != 0.0 && value != 1.0) => result = false
              })
            }
            result
          }
          val oldLowerBound = currentBranchNode.lowerBound
          val newLowerBound = currentBranchNode.computeLowerBound(currentBranchNode.lowerBoundSolve)
          if (newLowerBound <= oldLowerBound){
            currentBranchNode.iteration += 1
          }
          currentBranchNode.lowerBound = newLowerBound
          activeBranches = activeBranches ++ List(currentBranchNode)
          //print("Fractional solution, cuts found, lower bound is:"+currentBranchNode.lowerBound+"\r\n")
        } else {
          //println("Here2?")

          //print("No cuts found or attain max iteration for one node\r\n")
          // check if current solution is integer
          if (currentBranchNode.isInteger) {
            //print("current node is integer\r\n")
            // if integer, update current best solution
            currentBestNode = Some(currentBranchNode)
            activeBranches = activeBranches.filter(_.lowerBound <= currentBestNode.get.lowerBound)
          } else {
            // otherwise, branch and add to the branch list
            //print("current node is fractional\r\n")

            val children: List[BranchNode] = branchingScheme.listChildren(currentBranchNode)
            //print("children created\r\n")

            for (child <- children) {
              if (currentBestNode.isEmpty) {
                //println("add this children", child, "num of cuts: "+child.globalConstraints.size)
                activeBranches = activeBranches ++ List(child)
              } else if (child.lowerBound < currentBestNode.get.lowerBound) { //first check a naive lower bound for child node
                //println("add this children", child, "num of cuts: "+child.globalConstraints.size)
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
