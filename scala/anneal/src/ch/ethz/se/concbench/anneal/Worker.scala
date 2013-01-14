package ch.ethz.se.concbench.anneal

import akka.actor.ActorRef
import akka.actor.Actor
import scala.util.Random

class Worker(router: ActorRef, master: ActorRef) extends Actor {
  sealed abstract class WorkerState
  case class InitialRandom() extends WorkerState
  case class AnotherRandom() extends WorkerState

  sealed abstract class Decision
  case class Good() extends Decision
  case class Bad() extends Decision
  case class Deny() extends Decision

  val map = Main.map
  val size = Main.size
  
  val outerIters = 1000
  val innerIters = 32

  var good = 0
  var bad = -1
  var temper = 200.0
  var state: WorkerState = InitialRandom()
  var j = 0
  var i = 0
  var a: City = null
  var b: City = null

  def randomIndex: Int = Random.nextInt (size)

  def receive = {
    case Start() => {
      state = InitialRandom ()
      router ! GetCity (randomIndex)
    }
    case HeresCity(c) => {
      state match {
        case InitialRandom() => {
          a = c
          loop
        }
        case AnotherRandom() => {
          b = c
          var deltaDist = calcDist
          var decision = decide (deltaDist, temper)

          decision match {
            case Good() => {
              good = good + 1
              router ! Swap (a, b)
            }
            case Bad() => {
              bad = bad + 1
              router ! Swap (a, b)
            }
            case Deny() =>
          }
          a = b
          loop
        }
      }
    }
  }

  def decide(dist: Double, temper: Double): Decision = {
    val rnd = Random.nextDouble;

    if (dist < 0)
      Good ()
    else if (scala.math.exp(-dist / temper) > rnd)
      Bad ()
    else
      Deny ()
  }

  def calcDist: Double = {
    def singleDist(a: City): Double = {
      var dist = 0.0
      if (a.leftId != -1)
        dist += map(a.leftId, a.id)
      if (a.rightId != -1)
        dist += map(a.id, a.rightId)
      dist
    }

    val newA = new City(a.id, b.idxInRoute, b.leftId, b.rightId)
    val newB = new City(b.id, a.idxInRoute, a.leftId, a.rightId)

    var dist = 0.0
    dist += singleDist (newA) + singleDist (newB)
    dist -= singleDist (a) + singleDist (b)

    dist
  }

  def loop: Unit = {
    if (keepGoing (i)) {
      if (j < innerIters) {
        state = new AnotherRandom
        j = j + 1
        router ! GetCity (randomIndex)
      } else {
        j = 0
        temper = cool (temper)
        i = i + 1
        loop
      }
    } else {
      master ! Done()
    }
  }

  def cool(temp: Double): Double = temp / 1.5
  def keepGoing(i: Int): Boolean = i < outerIters
}