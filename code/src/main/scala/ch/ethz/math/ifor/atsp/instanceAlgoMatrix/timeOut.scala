package ch.ethz.math.ifor.atsp.instanceAlgoMatrix

import ch.ethz.math.ifor.atsp.{Input, Output, Runtime}

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object timeOut extends App {


  /** runs solver and returns (optimal value, running time) or DNF if it timed out or ERROR if there was an error
    *
    * @param maxTime
    * @param input
    * @param solver
    * @return
    */
  def timed(maxTime:Int, input:Input, solver: Input=> Output ): Either[(Double,Runtime), String]= {
    try {
      val timeout = maxTime.seconds
      val temp = try {
        val start = System.nanoTime
        val output = Await.result(Future {
          solver(input)
        }, timeout)
        (output.value, Runtime( (System.nanoTime - start) / 1e9d) )
      }
      catch {
        case _: TimeoutException => {
          return Right("DNF")
        }
      }
      Left(temp)
    }
    catch {
      case _: Exception => {
        Right("ERROR")
      }
    }
  }









  def timedToy(solver: Int=> Double, input:Int, maxTime:Int): Option[Double]= {
    val timeout = maxTime.seconds
    val temp = try {Await.result(Future{solver(input)},timeout)}
    catch {case _:TimeoutException => {
      println("Computation timed out!")
      return None}
    }
    Some(temp)
  }

  def fun(x:Int, y:Int)= {Thread.sleep(6500)
    x+0.5+10*y}

  val res = timedToy(fun(_,0), 1, 5)

}
