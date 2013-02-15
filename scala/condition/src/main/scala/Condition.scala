import scala.Array.canBuildFrom
import scala.collection.mutable.Queue

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.actorRef2Scala

object Condition {
  var maxElems: Int = 5000
  var numWorkers = 32

  def main(args: Array[String]) = {
    maxElems = args(0).toInt
    numWorkers = args(1).toInt

    val system = ActorSystem("ProdCons")
    val t1 = System.currentTimeMillis()


    val evens = new Array[ActorRef](numWorkers)
    val odds = new Array[ActorRef](numWorkers)
    val shared = system.actorOf(Props[Shared], name = "shared")

    for (i <- 0 until numWorkers) {
      evens(i) = system.actorOf(Props[Evener], name = "producer" + i)
      odds(i) = system.actorOf(Props[Odder], name = "consumer" + i)
    }

    evens map (_ ! Start(shared))
    odds map (_ ! Start(shared))

    system.awaitTermination
    val t2 = System.currentTimeMillis()
    println ((t2 - t1) / 1000.0)
  }

  sealed class Action
  case class Start(shared: ActorRef) extends Action
  case class RegisterEven() extends Action
  case class RegisterOdd() extends Action
  case class Put(i: Int, isLast: Boolean) extends Action
  case class IsEven(i: Int) extends Action
  case class IsOdd(i: Int) extends Action
  case class Stop() extends Action

  // Wants to get odd numbers, puts back even numbers
  class Odder extends Actor {
    var count = 0

    def receive = {
      case Start(shared: ActorRef) => {
        shared ! RegisterOdd()
      }

      case IsOdd(i: Int) => {
        count = count + 1
        sender ! Put(i + 1, count == maxElems)
        if (count == maxElems) {
          sender ! Stop()
          context.stop(self)
        }
      }
    }
  }

  class Evener extends Actor {
    var count = 0

    def receive = {
      case Start(shared: ActorRef) => {
        shared ! RegisterEven()
      }

      case IsEven(i: Int) => {
        count = count + 1
        sender ! Put(i + 1, count == maxElems)
        if (count == maxElems) {
          sender ! Stop()
          context.stop(self)
        }
      }
    }
  }

  class Shared extends Actor {
    var odds: Queue[ActorRef] = new Queue[ActorRef]()
    var evens: Queue[ActorRef] = new Queue[ActorRef]()
    var n = 0
    var valueOut = false
    var doneCount = 0

    def receive = {
      case RegisterOdd() => {
        odds.enqueue(sender)
        if (!valueOut && n % 2 == 1) {
          sender ! IsOdd(n)
          valueOut = true
        }
      }

      case RegisterEven() => {
        evens.enqueue(sender)
        if (!valueOut && n % 2 == 0) {
          sender ! IsEven(n)
          valueOut = true
        }
      }

      case Put(i: Int, isDone) => {
        assert(valueOut)
        n = i
        if (n % 2 == 1) {
          if (!odds.isEmpty) {
            val odd = odds.dequeue
            odd ! IsOdd(n)
            odds.enqueue(odd)
          } else {
            valueOut = false
          }
        } else {
          if (!evens.isEmpty) {
            val even = evens.dequeue
            even ! IsEven(n)
            evens.enqueue(even)
          } else {
            valueOut = false
          }
        }
        if (isDone) {
        	odds = odds.filter (_ != sender).toQueue
        	evens = evens.filter (_ != sender).toQueue
        }
      }

      case Stop() => {
        doneCount = doneCount + 1
        if (doneCount == 2 * numWorkers) {
          context.system.shutdown
        }
      }
    }
  }
}
