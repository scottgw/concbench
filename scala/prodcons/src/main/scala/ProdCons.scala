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
  implicit val timeout = Timeout(1 minutes)

  final val maxElems: Int = 20000
  val numWorkers = 32

  def main(args: Array[String]) = {
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
    println("System done")
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
        println("Producer done")

        context.stop(self)
      }
    }
  }
// // Trying to use futures, times out above 16 consuemrs.producers
//  class Consumer extends Actor {
//    var count = 0
//
//    def receive = {
//      case Start(shared) => {
//
//        for (i <- 0 until maxElems) {
//        	println("Consumer asking " + i)
//
//          val future = shared ? RequestElement()
//
//          Await.result(future, timeout.duration).asInstanceOf[Any]
//          println("Consumer received " + i)
//        }
//        shared ! Stop()
//        context.stop(self)
//      }
//    }
//  }
//
//  class Shared extends Actor {
//    val q: Queue[Any] = new Queue[Any]()
//    val waiters: Queue[ActorRef] = new Queue[ActorRef]()
//
//    var doneCount = 0
//
//    def receive = {
//      case RequestElement() => {
//        if (!q.isEmpty) {
//          sender ! q.dequeue
//        } else {
//          println("waiter size: " + waiters.size)
//
//          waiters.enqueue(sender)
//        }
//      }
//      case NewElement(a) => {
//        q.enqueue(a)
////        println("q size: " + q.size)
//
//        while (!waiters.isEmpty && !q.isEmpty) {
//          println("waiter size: " + waiters.size)
//          waiters.dequeue ! q.dequeue
//        }
//      }
//      case Stop() => {
//        doneCount = doneCount + 1
//        println("done: " + doneCount + " of " + 2 * numWorkers)
//        if (doneCount == 2 * numWorkers) {
//          println("Shared done")
//          context.system.shutdown
//        }
//      }
//    }
//  }
    class Consumer extends Actor {
      var count = 0
  
      def receive = {
        case Start(shared) => {
          shared ! RequestElement()
        }
        case ElementReady(a: Any) => {
          count = count + 1
          if (count == maxElems) {
            println ("Consumer done")
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
          println ("done: " + doneCount + " of " + 2*numWorkers)
          if (doneCount == 2*numWorkers) {
            println ("Shared done")
            context.system.shutdown
          }
        }
      }
    }
}
