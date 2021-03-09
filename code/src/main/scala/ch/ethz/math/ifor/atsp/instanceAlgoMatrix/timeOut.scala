package ch.ethz.math.ifor.atsp.instanceAlgoMatrix

import ch.ethz.math.ifor.atsp.{Input, Output, Runtime}

import scala.util.Try
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object timeOut {

  def custom_firstCompletedOf[T](futures: TraversableOnce[Future[T]]): Future[T] = {
    val p = Promise[T]()
    val pref = new java.util.concurrent.atomic.AtomicReference(p)
    val completeFirst: Try[T] => Unit = { result: Try[T] =>
      val promise = pref.getAndSet(null)
      if (promise != null) {
        promise.tryComplete(result)
      }
    }
    futures foreach { _ onComplete completeFirst }
    p.future
  }



  /** runs solver and returns (optimal value, running time) or DNF if it timed out or ERROR if there was an error
    *
    * @param maxTime
    * @param input
    * @param solver
    * @return
    */
  def timed(maxTime:Int, input:Input, solver: Input=> Output ): Either[(Double,Runtime), String]= {
    try {
      val fut = custom_firstCompletedOf(List(
        Future {
          val start = System.nanoTime
          val output = solver(input)
          val dur = Runtime((System.nanoTime - start) / 1e9d)
          Left(output.value, dur)
        },
        Future {
          Thread.sleep(maxTime * 1000)
          Right("DNF")
        }
      )
      )
      Await.result(fut, Duration.Inf)
    }

    catch {
      case e: Exception => Right("ERROR")
      }
    }


//      val timeout = maxTime.seconds
//      val temp = try {
//        val start = System.nanoTime
//        val output = Await.result(Future {
//          solver(input)
//        }, timeout)
//        (output.value, Runtime( (System.nanoTime - start) / 1e9d) )
//      }
//      catch {
//        case _: TimeoutException => {
//          return Right("DNF")
//        }
//      }
//      Left(temp)
//    }
//    catch {
//      case _: Exception => {
//        Right("ERROR")
//      }
//    }
}
