package ch.ethz.se.concbench.anneal

import scala.util.Random

class Map (size: Int, maxDist: Double) {
  def dist (a: City, b: City) = map (a.id)(b.id)
  def apply (i: Int, j: Int) = map (i)(j)
  val map = Array.fill (size, size) (Random.nextDouble() * maxDist)
}