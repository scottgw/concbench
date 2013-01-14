package ch.ethz.se.concbench.anneal

import akka.actor._

object Main extends App {
  val numWorkers = args (0).toInt
  val size = 1000
  val maxDist = 200.0
  val map: Map = new Map(size, maxDist)
  val numRuns = 10

  val t1 = System.currentTimeMillis().toDouble
  for (i <- 0 until numRuns) {
    run
  }
  val t2 = System.currentTimeMillis().toDouble
  val time = (t2 - t1) / 1000
  println (time / numRuns + " s")
  
  def run = {
    val system = ActorSystem ("AnnealSystem")
    val workers: Array[ActorRef] = new Array(numWorkers)
    val master = system.actorOf(Props(new Master(workers)), name = "master")
    val router = system.actorOf(Props(new Router(size, map)), name = "router")

    for (i <- 0 until numWorkers) {
      val worker = system.actorOf(
        Props(new Worker(router, master)),
        name = "worker" + i)
      workers(i) = worker
    }
    master ! MasterStart()
    system.awaitTermination
  }

}

class Master(workers: Array[ActorRef]) extends Actor {
  var numDone: Int = 0

  def receive = {
    case Done() => {
      numDone += 1
      if (numDone == workers.size) {
        context.system.shutdown
      }
    }
    case MasterStart() => {
      for (w <- workers) {
        w ! Start ()
      }
    }
  }
}

sealed trait RouterMessage
case class Swap(a: City, b: City) extends RouterMessage
case class GetCity(i: Int) extends RouterMessage

sealed trait WorkerMessage
case class HeresCity(c: City) extends WorkerMessage
case class Start() extends WorkerMessage

sealed trait MasterMessage
case class MasterStart() extends MasterMessage
case class Done() extends MasterMessage

class Router(size: Int, map: Map) extends Actor {
  val route = new Route(size, map)

  def receive = {
    case Swap(a, b) => route.swap (a, b)
    case GetCity(i) => sender ! HeresCity (route (i))
  }
}