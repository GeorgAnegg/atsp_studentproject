package ch.ethz.math.ifor.atsp.instanceAlgoMatrix

import java.util.{Timer, TimerTask}
import java.util.concurrent.atomic.AtomicReference

import ch.ethz.math.ifor.atsp.{Input, Output, Runtime}

import scala.language.postfixOps
import scala.util.Try
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object timeOut {


  def notTimed(maxTime: Int, input:Input, solver: Input=> Output): Either[(Double,Runtime), String] ={
    try {
      lazy val start = System.nanoTime
      lazy val output = solver(input)
      lazy val dur = Runtime((System.nanoTime - start) / 1e9d)
      lazy val temp = Left(output.value, dur)
      temp
    }
    catch {
      case e: OutOfMemoryError => {System.gc()//suggest garbage collection
        Right("DNF")}
      case e: Exception => Right("ERROR")
    }
  }

  /** returns a custom future that is interruptable
    *
    * @param fun
    * @tparam T
    * @return
    */
  def interruptableFuture[T](fun: () => T): (Future[T], () => Boolean) = {
    val p = Promise[T]()
    val f = p.future
    val aref = new AtomicReference[Thread](null)
    p tryCompleteWith Future {
      val thread = Thread.currentThread
      aref.synchronized { aref.set(thread) }
      try fun() finally {
        val wasInterrupted = aref.synchronized { aref getAndSet null }
      }
    }
    (f, () => {
      aref.synchronized { Option(aref getAndSet null) foreach { _.interrupt() } }
      p.tryFailure(new CancellationException)
    })
  }





  /** this is supposed to remove references to ongoing future and let it be garbage collected, but it didn't make a difference
    *
    * @param futures
    * @tparam T
    * @return
    */
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



  def after[T](duration: Duration)(block: => T): Future[T] = {
    val promise = Promise[T]()
    val t = new Timer()
    t.schedule(new TimerTask {
      override def run(): Unit = {
        promise.complete(Try(block))
      }
    }, duration.toMillis)
    promise.future
  }


  def timed2(maxTime:Int, input: Input, solver: Input=> Output):  Either[(Double,Runtime), String] ={
    try {
      lazy val (compute,cancel) = interruptableFuture[Either[(Double,Runtime), String]] { () =>
        lazy val start = System.nanoTime
        lazy val output = solver(input)
        lazy val dur = Runtime((System.nanoTime - start) / 1e9d)
        lazy val temp = Left(output.value, dur)
        temp
      }
      after(maxTime seconds)({println("computation timed out")
        cancel()})
      /*
      val sleep = Future{
        Thread.sleep(maxTime*1000)
        println("computation timed out")
        cancel()
        Right("DNF")}

      val fut = Future.firstCompletedOf(List(
        compute, sleep))
*/
      val temp = Await.result(compute, Duration.Inf)
      return temp
    }

    catch {
      case e: CancellationException => Right("DNF")
        case e: Exception => {println(e)
          Right("ERROR")
      }
    }


  }



  /** runs solver and returns (optimal value, running time) or DNF if it timed out or ERROR if there was an error
    *
    * @param maxTime
    * @param input
    * @param solver
    * @return
    */
  def timed(maxTime:Int, input:Input , solver: Input=> Output): Either[(Double,Runtime), String]= {
    try {

      val (compute,cancel) = interruptableFuture[Either[(Double,Runtime), String]] { () =>
        val start = System.nanoTime
        val output = solver(input)
        val dur = Runtime((System.nanoTime - start) / 1e9d)
        Left(output.value, dur)
      }
      val sleep = Future{
        Thread.sleep(maxTime*1000)
        println("computation timed out")
        cancel()
        Right("DNF")}

      val fut = Future.firstCompletedOf(List(
        compute, sleep))

      val temp = Await.result(fut, Duration.Inf)
      temp
    }

    catch {
      case e: Exception => {println(e)
        Right("ERROR")
      }
    }
  }






  /*
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
*/

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
