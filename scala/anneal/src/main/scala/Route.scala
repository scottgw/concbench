package ch.ethz.se.concbench.anneal

class Route (size: Int, map: Map){
  private val route : Array[City] = Array.fill (size) (null)
  
  for (i <- 0 until size)
    route(i) = new City (i, i, i-1, i+1)
  
  def swap (a: City, b: City) = {
    val i = a.idxInRoute
    val j = b.idxInRoute
    
    val tmp = route (i)
    route (i) = route(j)
    route (j) = tmp
  }
  
  def apply (i: Int): City = {
    val c = route (i)
    
    val leftId = if (i == 0) (-1) else route(i-1).id
    val rightId = if (i == size - 1) (-1) else route(i+1).id
    
    new City (c.id, i, leftId, rightId)
  }
  
  
}