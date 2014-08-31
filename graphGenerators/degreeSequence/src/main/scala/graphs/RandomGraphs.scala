package learn.graph
import scala.util.Random._

object RandomGraphs{
  /**
    * Keeping this simple, graphs are generated from list of (Int, Int)
    * where Int labels a vertex
    * Use implicit conversion from Int to vertex type ( if not an Int )
    * (x,y)'s equivalence to (y, x) is taken care of explicitly. 
    * Use implicit conversion from (Int, Int) to E[Int] to generate Graphs
    */
  def linksErdosRenyi_prob(N: Int, p: Double): Seq[ (Int, Int) ] =
    for { i <- 0 to N; j <- i + 1 until N; if (nextDouble < p )} yield (i, j)

  def linksErdosRenyi_number(N: Int, M: Int): Seq[ (Int, Int) ] = {
    val allLinks = for{x <- Range(0, N); y <- Range(x+1, N)} yield (x, y)
    shuffle(allLinks).take(M)//simple, straightforward, could be made faster
  }
}

