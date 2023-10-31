import scala.language.postfixOps
import scala.io.StdIn
import scala.util._
import scala.util.control.NonFatal
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{async, await}

package object nodescala {

  // Future Companion Object Extensions

  implicit class FutureCompanionOps(val f: Future.type) extends AnyVal {

    // Returns a future that is always completed with `value`.
    def always[T](value: T): Future[T] = Future.successful(value)

    // Returns a future that is never completed.
    def never[T]: Future[T] = {
      val promise = Promise[T]()
      promise.future
    }

    // Given a list of futures `fs`, returns the future holding the list of values of all the futures from `fs`.
    def all[T](fs: List[Future[T]]): Future[List[T]] = Future.sequence(fs)

    // Given a list of futures `fs`, returns the future holding the value of the future from `fs` that completed first.
    def any[T](fs: List[Future[T]]): Future[T] = Future.firstCompletedOf(fs)

    // Returns a future with a unit value that is completed after time `t`.
    def delay(t: Duration): Future[Unit] = {
      Future {
        blocking {
          Thread.sleep(t.toMillis)
        }
      }
    }

    // Completes this future with user input.
    def userInput(message: String): Future[String] = Future {
      blocking {
        StdIn.readLine(message)
      }
    }

    // Creates a cancellable context for an execution and runs it.
    def run()(f: CancellationToken => Future[Unit]): Subscription = {
      val cancellationTokenSource = CancellationTokenSource()
      f(cancellationTokenSource.cancellationToken)
      cancellationTokenSource
    }

  }

  // Future Extensions

  implicit class FutureOps[T](val f: Future[T]) extends AnyVal {

    // Returns the result of this future if it is completed now.
    def now: T = f.value match {
      case Some(Success(result)) => result
      case Some(Failure(cause)) => throw cause
      case None => throw new NoSuchElementException("Future is not completed")
    }

    // Continues the computation of this future by taking the current future and mapping it into another future.
    def continueWith[S](cont: Future[T] => S): Future[S] = {
      f.flatMap(_ => Future(cont(f)))
    }

    // Continues the computation of this future by taking the result of the current future and mapping it into another future.
    // Continues the computation of this future by taking the result of the current future and mapping it into another future.
    def continue[S](cont: Try[T] => S): Future[S] = {
      f.map(result => cont(Success(result))).recover {
        case NonFatal(e) => cont(Failure(e))
      }
    }


  }

  // Subscription

  trait Subscription {
    def unsubscribe(): Unit
  }

  object Subscription {

    // Given two subscriptions `s1` and `s2` returns a new composite subscription
    def apply(s1: Subscription, s2: Subscription): Subscription = new Subscription {
      def unsubscribe(): Unit = {
        s1.unsubscribe()
        s2.unsubscribe()
      }
    }
  }

  // CancellationToken

  trait CancellationToken {
    def isCancelled: Boolean
    def nonCancelled: Boolean = !isCancelled
  }

  // CancellationTokenSource

  trait CancellationTokenSource extends Subscription {
    def cancellationToken: CancellationToken
  }

  object CancellationTokenSource {

    // Creates a new `CancellationTokenSource`.
    def apply(): CancellationTokenSource = new CancellationTokenSource {
      val p = Promise[Unit]()
      val cancellationToken = new CancellationToken {
        def isCancelled: Boolean = p.future.value != None
      }

      def unsubscribe(): Unit = {
        p.trySuccess(())
      }
    }
  }
}
