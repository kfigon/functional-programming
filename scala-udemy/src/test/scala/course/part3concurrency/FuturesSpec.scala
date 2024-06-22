package course.part3concurrency

import org.scalatest.funspec.AnyFunSpec

import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

class FuturesSpec extends AnyFunSpec {
  it("thread") {
    val t = new Thread(() => {
      println("thread run")
      42
    }) // JVM thread => OS thread
    t.start()
    t.join() // block until t finishes
  }

  it("future") {
    val f = Future(42) // need implicit execution context
    val r = f.map(_ + 8)
    val out = Await.result(r,Duration(1, TimeUnit.SECONDS)) // ready returns a future with Done. Result throws exception

    assert(out == 50)
  }
//  for filter we need recover, as not founds will fail the future with exception
//  recover - takes partial fun and returns val
//  recoverWith - takes partial fun and returns future
//  fallbackTo - return a fallback future value. It'll return a val in case of failure, no need to write partial funs

  it("future on complete") {
    val f = Future(42)
    f.onComplete { // this will be done by some other thread. Just use maps and for comprehensions
      case Failure(exception) => println("fail", exception)
      case Success(value) => println("ok", value)
    }
  }
}
