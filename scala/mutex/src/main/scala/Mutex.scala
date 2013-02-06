import akka.actor.ActorSystem
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props

object Mutex {
  var maxCount = 20000
  var numWorkers = 32

  abstract class Action
  sealed case class Start(shared: ActorRef) extends Action
  
  def main(args: Array[String]) = {
    val system = ActorSystem ("Mutex")
    val workers = new Array[ActorRef](numWorkers)
    val shared = system.actorOf(Props[Shared], name = "shared")
    
    for (i <- 0 until numWorkers) {
      workers(i) = system.actorOf(Props[Worker], name = "worker" + i)
      workers(i) ! Start (shared)
    }
        
    system.awaitTermination
    println ("System done")
  }

  class Shared extends Actor {
    var count = 0
    def receive = {
      case _ => {
        count = count + 1
        if (count == maxCount * numWorkers)
          context.system.shutdown
      }
    }
  }

  class Worker extends Actor {
    def receive = {
      case Start(shared) => {
        for (i <- 0 until maxCount) {
          shared ! ()
        }
      }
    }
  }
}