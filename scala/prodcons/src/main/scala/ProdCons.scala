import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRef
import akka.pattern.ask
import scala.collection.mutable.Queue

import akka.dispatch.Await
import akka.util.Timeout
import akka.util.duration._

object ProdCons {
  // implicit val timeout = Timeout(1 minutes)

  var maxElems: Int = 20000
  var numWorkers = 32

  def main(args: Array[String]) = {
    maxElems = args(0).toInt
    numWorkers = args(1).toInt

    val system = ActorSystem("ProdCons")
    val producers = new Array[ActorRef](numWorkers)
    val consumers = new Array[ActorRef](numWorkers)
    val shared = system.actorOf(Props[Shared], name = "shared")

    for (i <- 0 until numWorkers) {
      producers(i) = system.actorOf(Props[Producer], name = "producer" + i)
      consumers(i) = system.actorOf(Props[Consumer], name = "consumer" + i)
    }

    producers map (_ ! Start (shared))
    consumers map (_ ! Start (shared))
    
    system.awaitTermination
  }

  sealed class Action
  case class Start(shared: ActorRef) extends Action
  case class ElementReady(a: Any) extends Action
  case class RequestElement() extends Action
  case class NewElement(a: Any) extends Action
  case class Stop() extends Action

  class Producer extends Actor {
    def receive = {
      case Start(shared: ActorRef) => {
        for (i <- 0 until maxElems) {
          shared ! NewElement(i)
        }
        shared ! Stop()

        context.stop(self)
      }
    }
  }

    class Consumer extends Actor {
      var count = 0
  
      def receive = {
        case Start(shared) => {
          shared ! RequestElement()
        }
        case ElementReady(a: Any) => {
          count = count + 1
          if (count == maxElems) {
            sender ! Stop()
            context.stop(self)
          } else {
            sender ! RequestElement()
          }
        }
      }
    }
  
    class Shared extends Actor {
      val q: Queue[Any] = new Queue[Any]()
      val waiters: Queue[ActorRef] = new Queue[ActorRef]()
  
      var doneCount = 0
      
      def receive = {
        case RequestElement() => {
          if (!q.isEmpty) {
            sender ! ElementReady (q.dequeue)
          } else {
            waiters.enqueue(sender)
          }
        }
        case NewElement(a) => {
          q.enqueue(a)
          while (!waiters.isEmpty && !q.isEmpty) {
            waiters.dequeue ! ElementReady (q.dequeue)
          }
        }
        case Stop () => {
          doneCount = doneCount + 1

          if (doneCount == 2*numWorkers) {
            context.system.shutdown
          }
        }
      }
    }
}
