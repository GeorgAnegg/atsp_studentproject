package ch.ethz.math.ifor.atsp.CompactFormulations

import ch.ethz.math.ifor.atsp.BranchAndCut.{BranchNode, linearProgrammingSolver}
import ch.ethz.math.ifor.atsp.{Input, Output, Site, Solver, Tour, arcWise, inf, negInf}
import com.google.ortools.linearsolver.{MPConstraint, MPSolver, MPVariable}

object MTZ_FracIslands extends Solver{

  def fractionalIslands(input: Input, relaxation:Map[Site,Map[Site,Double]],x:arcWise[MPVariable]):List[(Map[MPVariable,Double],Double)] = {
    var allSites: List[Site] = input.sites.toList
    var allEdges: Map[Site, List[Site]] = Map()
    allSites.foreach{e => allEdges =allEdges ++Map(e->List())}

    relaxation.collect {
      case (site1, map1) => map1.collect {
        case (site2, value) if value != 0 => allEdges = allEdges.updated(site1, allEdges(site1) ++ List(site2))
        //case (site2, value) if value != 0 => println(site2, site2, value)
      }
    }

    var allIslands: List[List[Site]] = List()
    var currentIsland: List[Site] = List()
    var visited : List[Site] = List()

    def findNeighbours(site: Site, list: List[Site]): (List[Site], List[Site]) = {
      var res2: List[Site] = List()
      var resList: List[Site] = List()
      val adj = allEdges(site)
      for (i <- adj) {
        if (!list.contains(i)) {
          res2 = res2 :+ i
        }
        if (!visited.contains(i)){
          resList = resList :+ i
        }
      }
      (resList, res2)
    }

    while (allSites.nonEmpty) {
      val currentSite = allSites.head
      visited = visited :+ currentSite

      var adjSites: List[Site] = allEdges(currentSite)
      if (adjSites.size > 1) {
        currentIsland = currentIsland :+ currentSite
        allSites = allSites.drop(1)

        while (adjSites.nonEmpty) {
          var added: List[Site] = List()
          for (e <- adjSites){
            visited = visited :+ e
            currentIsland = currentIsland :+ e
          }
          for (i <- adjSites) {
            val res = findNeighbours(i, currentIsland)
            currentIsland = currentIsland ++ res._2
            added = added ++ res._1
            //remove i from adj and allSites
            adjSites = adjSites.filterNot(_ == i)
            allSites = allSites.filterNot(_ == i)
          }
          adjSites = adjSites ++ added
        }
          allIslands = allIslands :+ currentIsland
          currentIsland = currentIsland.drop(currentIsland.size)
      }

      else {
        allSites = allSites.drop(1)
      }
      /*
      else if (allEdges(currentSite).length == 1){
        val toSite = allEdges(currentSite).head
        val toSiteNeighbours = allEdges(toSite)
        if (toSiteNeighbours.size==1){
          if (toSiteNeighbours.head==currentSite){
            currentIsland = currentIsland :+ currentSite
            currentIsland = currentIsland :+ toSite
            allSites = allSites.filterNot(_ == toSite)
            currentIsland = currentIsland.drop(currentIsland.size)
          }
        }
      }

       */

    }
    var resList: List[(Map[MPVariable,Double],Double)] = List()


    /*
    println("size of islands: ",allIslands.size)
    for (i<-allIslands){
      for (j<-i){
        print(j+"  ")
      }
      println("\r\n")
    }
     */
    if (allIslands.size==1 && allIslands.head.length>=input.sites.length-1){
      return List()
    }

    for (island <- allIslands) {
      var resultMap: Map[MPVariable, Double] = Map()
      for (node1 <- island) {
        for (node2 <- island) {
          resultMap = resultMap ++ Map(x.search(node1, node2) -> 1.0)
          resultMap = resultMap ++ Map(x.search(node2, node1) -> 1.0)
        }
      }
      resList = resList :+ (resultMap, island.size - 1)
    }
    resList
  }

  def solve (input: Input,formulation:String, preprocessing:Boolean,useAdditive:Boolean,useParametricAP:Boolean):Output={
    System.loadLibrary("jniortools")

    val solver: MPSolver = new MPSolver("",
      MPSolver.OptimizationProblemType.GLOP_LINEAR_PROGRAMMING)

    def constructVariable(site1:Site ,site2:Site):MPVariable= {
      if (site1.id==site2.id) {solver.makeNumVar(0,0,"")}
      else {
        solver.makeNumVar(0,1,"")
      }
    }

    val x: arcWise[MPVariable] = arcWise(input, constructVariable)
    val costs:arcWise[Double] = arcWise(input,input.distance)

    // construct order variables
    var orderVariables:Map[Site, MPVariable]=Map()
    input.sites.foreach {
      case site if (site!=input.sites.head)=> orderVariables += (site -> solver.makeNumVar(1, input.sites.length - 1, ""))
      case site if (site==input.sites.head)=> orderVariables += (site -> solver.makeNumVar(negInf, inf, ""))
    }

    // Each site has at most one out-degree.
    for (i <- input.sites) {
      val constraint = solver.makeConstraint(1, 1, "")
      for (j <- input.sites) {
        constraint.setCoefficient(x.search(i,j), 1)
        //println("Number of constraints = " + solver.numConstraints(),i,j,x.search(i,j))
      }
    }

    // Each site has at most one in-degree.
    for (j <- input.sites) {
      val constraint = solver.makeConstraint(1, 1, "")
      for (i <- input.sites) {
        constraint.setCoefficient(x.search(i,j), 1)
        //println("Number of constraints = " + solver.numConstraints(),i,j,x.search(i,j))
      }
    }

    // Add the order constraints
    for (i <- input.sites) {
      for (j <-input.sites){
        if (j != i && j!= input.sites.head){
          val constraint  = solver.makeConstraint(negInf,input.sites.length-2)
          constraint.setCoefficient(x.search(i,j),input.sites.length-1)
          constraint.setCoefficient(orderVariables(i),1)
          constraint.setCoefficient(orderVariables(j),-1)
        }
      }
    }

    // Create the objective function.
    val objective = solver.objective()
    for (i <- input.sites) {
      for (j <- input.sites) {
        objective.setCoefficient(x.search(i,j), costs.search(i,j))
      }
    }
    objective.setMinimization()

    def solveRelaxation(input:Input,vars:arcWise[MPVariable],solver:MPSolver):Map[Site,Map[Site,Double]] = {
      val result = solver.solve()
      println(result)
      def constructVariable(site1:Site ,site2:Site):Double={
        vars.search(site1, site2).solutionValue()
      }
      val resultSol:arcWise[Double] = arcWise(input,constructVariable)
      resultSol.entries
    }

    var constraintsInNode: List[MPConstraint] = List()

    def fromCutToConstraint(cuts:List[(Map[MPVariable,Double],Double)]): Unit ={
      for (cut <- cuts){
        val constraint : MPConstraint = solver.makeConstraint(negInf, cut._2, "")
        cut._1.foreach{
          case (mpVariable,coefficient) => constraint.setCoefficient(mpVariable,coefficient)
        }
        constraintsInNode = constraintsInNode ++ List(constraint)
      }
    }

    def isInteger(lowerBoundSolve:Map[Site,Map[Site,Double]]):Boolean = {
      var result = true
      lowerBoundSolve.collect{
        case (site1, map1) => (site1, map1.collect{
          case (site2, value) if (value != 0.0 && value != 1.0) => result = false
        })
      }
      result
    }

    def detectTours(lbSolve:Map[Site, Map[Site, Double]]):List[Tour] = {
      var pairMap = lbSolve.map({ case (site1, map1) => site1 -> map1.filter(_._2==1).head._1 })
      if (pairMap.size!=input.sites.length){
        return List()
      }
      //println("====pairmap init=====")
      //pairMap.foreach{e => println(e._1,e._2)}
      var currentList : List[Site] =List()
      var listTours: List[Tour] = List()
      var currentArc = pairMap.head
      currentList = currentList :+ currentArc._1
      currentList = currentList :+ currentArc._2
      pairMap = pairMap - currentArc._1

      while (pairMap.nonEmpty){
        //println("====pairmap status=====")
        //pairMap.foreach{e => println(e._1,e._2)}

        if (pairMap.contains(currentList.last)){
          val nextArc = pairMap.find(_._1==currentList.last).get
          if (nextArc._2!=currentList.head){
            currentList = currentList :+ nextArc._2
            pairMap = pairMap - nextArc._1
            currentArc = nextArc
          } else {
            pairMap = pairMap - nextArc._1
            //println("=========")
            //currentList.foreach{e => println(e)}
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
          //println("=========")
          //currentList.foreach{e => println(e)}
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

    // start
    var currentSolution: Map[Site, Map[Site, Double]] = solveRelaxation(input, x, solver)
    currentSolution.foreach{
      case (site1, map1) => map1.foreach{
        case (site2, value) => println(site1, site2, value)
      }
    }

    while (!isInteger(currentSolution) || detectTours(currentSolution).length!=1){
      val cuts  = fractionalIslands(input, currentSolution,x)
      if (cuts.nonEmpty){
        for (cut<-cuts){
          fromCutToConstraint(List(cut))
        }
      }
      currentSolution = solveRelaxation(input, x, solver)
    }

    val toursInSolution = detectTours(currentSolution)
    new Output(input, toursInSolution.head)
  }





  }















