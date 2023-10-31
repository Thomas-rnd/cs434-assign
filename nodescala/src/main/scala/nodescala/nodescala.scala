package nodescala

import com.sun.net.httpserver._
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{async, await}
import scala.collection._
import scala.collection.JavaConversions._
import java.util.concurrent.{Executor, ThreadPoolExecutor, TimeUnit, LinkedBlockingQueue}
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import java.net.InetSocketAddress
import scala.util.{Success, Failure}

/** Contains utilities common to the NodeScala© framework.
 */
trait NodeScala {
  import NodeScala._

  def port: Int

  def createListener(relativePath: String): Listener

  /** Uses the response object to respond to the write the response back.
   *  The response should be written back in parts, and the method should
   *  occasionally check that server was not stopped, otherwise a very long
   *  response may take very long to finish.
   *
   *  @param exchange     the exchange used to write the response back
   *  @param token        the cancellation token
   *  @param body         the response to write back
   */
  private def respond(exchange: Exchange, token: CancellationToken, response: Response): Unit = {
    // Helper function to write response in parts
    def writeResponsePart(part: String): Unit = {
      exchange.write(part)
      // Check if the token is canceled
      if (token.isCancelled) {
        // If canceled, close the exchange and stop writing
        exchange.close()
      }
    }

    // Iterate through the response and write it in parts
    while (response.hasNext && !token.isCancelled) {
      val part = response.next()
      writeResponsePart(part)
    }

    // Close the exchange after the response is fully sent
    exchange.close()
  }

  /** A server:
   *  1) creates and starts an http listener
   *  2) creates a cancellation token (hint: use one of the `Future` companion methods)
   *  3) as long as the token is not cancelled and there is a request from the http listener,
   *     asynchronously process that request using the `respond` method
   *
   *  @param relativePath   a relative path on which to start listening
   *  @param handler        a function mapping a request to a response
   *  @return               a subscription that can stop the server and all its asynchronous operations *entirely*
   */
  def start(relativePath: String)(handler: Request => Response): Subscription = {
    val listener = createListener(relativePath)
    val server = listener.start()
    val tokenSource = CancellationTokenSource()

    // Define a recursive function to handle incoming requests
    def handleRequests(): Unit = {
      val requestFuture = listener.nextRequest()

      requestFuture.onComplete {
        case Success((request, exchange)) =>
          // Handle the request using the provided handler
          val response = handler(request)
          // Respond asynchronously
          Future {
            respond(exchange, tokenSource.cancellationToken, response)
            // Check if the token is canceled, and if not, handle more requests
            if (!tokenSource.cancellationToken.isCancelled) {
              handleRequests()
            }
          }
        case Failure(_) =>
        // Failed to get a request, stop handling requests
      }
    }

    // Start handling requests
    handleRequests()

    new Subscription {
      def unsubscribe(): Unit = {
        // Cancel the token and stop the server
        tokenSource.unsubscribe()
        server.unsubscribe()
      }
    }
  }

}


object NodeScala {

  /** A request is a multimap of headers, where each header is a key-value pair of strings.
   */
  type Request = Map[String, List[String]]

  /** A response consists of a potentially long string (e.g. a data file).
   *  To be able to process this string in parts, the response is encoded
   *  as an iterator over a subsequences of the response string.
   */
  type Response = Iterator[String]

  /** Used to write the response to the request.
   */
  trait Exchange {
    /** Writes to the output stream of the exchange.
     */
    def write(s: String): Unit

    /** Communicates that the response has ended and that there
     *  will be no further writes.
     */
    def close(): Unit

    def request: Request

  }

  object Exchange {
    def apply(exchange: HttpExchange) = new Exchange {
      val os = exchange.getResponseBody()
      exchange.sendResponseHeaders(200, 0L)

      def write(s: String) = os.write(s.getBytes)

      def close() = os.close()

      def request: Request = {
        val headers = for ((k, vs) <- exchange.getRequestHeaders) yield (k, vs.toList)
        immutable.Map() ++ headers
      }
    }
  }

  trait Listener {
    def port: Int

    def relativePath: String

    def start(): Subscription

    def createContext(handler: Exchange => Unit): Unit

    def removeContext(): Unit

    /** This Method:
     *  1) constructs an uncompleted promise
     *  2) installs an asynchronous `HttpHandler` to the `server`
     *     that deregisters itself
     *     and then completes the promise with a request when `handle` method is invoked
     *  3) returns the future with the request
     *
     *  @return                the promise holding the pair of a request and an exchange object
     */
    def nextRequest(): Future[(Request, Exchange)] = {
      val p = Promise[(Request, Exchange)]()

      createContext(xchg => {
        val req = xchg.request
        removeContext()
        p.success((req, xchg))
      })

      p.future
    }
  }

  object Listener {
    class Default(val port: Int, val relativePath: String) extends Listener {
      private val s = HttpServer.create(new InetSocketAddress(port), 0)
      private val executor = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue)
      s.setExecutor(executor)

      def start() = {
        s.start()
        new Subscription {
          def unsubscribe() = {
            s.stop(0)
            executor.shutdown()
          }
        }
      }

      def createContext(handler: Exchange => Unit) = s.createContext(relativePath, new HttpHandler {
        def handle(httpxchg: HttpExchange) = handler(Exchange(httpxchg))
      })

      def removeContext() = s.removeContext(relativePath)
    }
  }

  /** The standard server implementation.
   */
  class Default(val port: Int) extends NodeScala {
    def createListener(relativePath: String) = new Listener.Default(port, relativePath)
  }

}
