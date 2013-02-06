package ch.ethz.se.concbench.noshare

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRef

object NoShare {
  val numWorkers: Int = 4

  sealed class Resp
  case class Answer(i: Long) extends Resp
  case class Start() extends Resp

  def main(args: Array[String]): Unit = {
    // numWorkers = args(1).toInt
    val system = ActorSystem("NoShare")
    val workers: Array[ActorRef] = new Array[ActorRef] (numWorkers)
    for (i <- 0 until numWorkers) {
      workers (i) = system.actorOf(Props[Fibber], name = "fibber" + i) 
    }
    
    class Master extends Actor {
      var cnt: Int = 0
      def receive = {
        case Answer(i) => {
          println (i)
          cnt = cnt + 1
          if (cnt == numWorkers)
            context.stop(self)
        }
        case Start() => {
          for (i: Int <- 0 until numWorkers) {
            (workers(i)) ! ()
          }
        }
      }
    }
    
    val master = system.actorOf(Props(new Master), name = "master")
    master ! Start()
    system.awaitTermination;
  }

  class Fibber extends Actor {
    def receive = {
      case _ => {
        sender ! Answer (fib(40))
        context.stop(self)
      }
    }

    def fib(i: Long): Long = {
      if (i <= 1)
        return i
      else
        return fib(i - 1) + fib(i - 2)
    }
  }
}