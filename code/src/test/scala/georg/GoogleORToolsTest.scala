package georg

import com.google.ortools.linearsolver.MPSolver

object GoogleORToolsTest extends App {

  System.loadLibrary("jniortools")

  val solver: MPSolver = new MPSolver("SimpleMIP",
    MPSolver.OptimizationProblemType.CBC_MIXED_INTEGER_PROGRAMMING)

  val inf = java.lang.Double.POSITIVE_INFINITY

  val x = solver.makeIntVarArray(5, 0, inf)
  x.foreach(println(_))


  val z = solver.makeIntVar(0, inf, "z")
  val y = solver.makeIntVar(0, inf, "y")

  val c0 = solver.makeConstraint(0, 100.5, "c0")
  c0.setCoefficient(z, 1)
  val c1 = solver.makeConstraint(20, 50.5, "c1")
  c1.setCoefficient(y, 1)


  val obj = solver.objective

  obj.setMaximization()
  obj.setCoefficient(z, 1)
  obj.setCoefficient(y, 1)
  obj.setCoefficient(y, 3) //this overwrites the existing coefficient

  val resStatus = solver.solve

  println(s"x = ${z.solutionValue}")
  println(s"y = ${y.solutionValue}")
  println("y coeff = " + obj.getCoefficient(y))
}
