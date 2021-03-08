package ch.ethz.math.ifor.atsp.CompactFormulations
import ch.ethz.math.ifor.atsp.BranchAndBound.BranchNode
import ch.ethz.math.ifor.atsp.{Input, Output, Site, Tour, arcWise, inf, negInf}
import com.google.ortools.linearsolver.{MPSolver, MPVariable}

object MTZ2020 extends CompactFormulation {
  def solve(input: Input): (Map[Site,Map[Site,Boolean]],Output)={

    System.loadLibrary("jniortools")

    val solver: MPSolver = new MPSolver("",
      MPSolver.OptimizationProblemType.CBC_MIXED_INTEGER_PROGRAMMING)

    // x(i)(j) is an array of 0-1 variables, which will be 1
    // if site i is assigned to site j.
    //val x = Array.ofDim[MPVariable](numSites, numSites)

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

    //orderVariables.foreach(a => print(a._1, a._2, "\r\n"))

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
    // println("num variables",solver.numVariables())
    objective.setMinimization()

    val resultStatus = solver.solve()
    println("Result status: "+resultStatus)

    def constructResult(site1:Site ,site2:Site):Boolean= {
      if (x.search(site1,site2).solutionValue == 1) {true}
      else {false}
    }

    /*

    for (i <- input.sites) {
      for (j <- input.sites) {
        print("site: ",i, " to site: ",j," with cost ",x.search(i,j).solutionValue+"\r\n")
      }
    }

    for (i<-input.sites){
      print("order of ",i," is ",orderVariables(i).solutionValue()+"\r\n")
    }

     */

    val resultArray: arcWise[Boolean] = arcWise(input, constructResult)

    var listSites : List[Site] = List()
    resultArray.entries.foreach{
      case (site1, map1) => (site1, map1.foreach{
        case (site2, value) if value => listSites = listSites :+ site1
      })
    }
    val tour = new Tour(input, listSites)
    val optOutput = new Output(input, tour)

    (resultArray.entries,optOutput)

  }


}
