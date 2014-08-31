package learn.graph
import scala.util.Random._


/**
  * traverse a graph of nodes of type V
  * to create a list of edges followed
  */
trait GraphTraversal[V] {
  type E = (V, V)
  def nbrs: V => Seq[V]
  def insert(zs:  List[E])(ys: (V, V)*):  List[(V, V)]

  def grow(axs: List[E] , queued: Set[V]): List[E]  = axs match{
    case Nil => Nil
    case (a, x) :: azs => {val ys = nbrs(x).filter ( !queued(_) );
      (a, x) :: grow( insert(azs)(ys.map( (x, _)): _*), queued ++ ys.toSet )
    }
  }
  
 def from( x: V): List[V] = grow( List( (x, x) ), Set(x)).map(_._2)
 def fromWithAncestors(x: V): List[E] = grow( List( (x, x) ), Set(x) )


}



class BreadthFirstTraversal[V](val nbrs: V => List[V]) extends GraphTraversal[V] {
  def insert(zs: List[E])( ys: E*): List[E] = zs ++ ys


  def shortestPathsFrom(x: V): Map[V, List[V]] = { 
    val revPaths = (Map[V, List[V]](x -> List(x)) /: this.fromWithAncestors(x).tail){
                      case (paths, (a, y)) => paths.updated( y, y :: paths(a))
    }
    revPaths.mapValues(_.reverse)
  }
}

object BreadthFirstTraversal {
  def apply[T](nbrs: T => List[T]): BreadthFirstTraversal[T] = new BreadthFirstTraversal(nbrs)
  def apply[T](adjlist: Map[T, IndexedSeq[T]]): BreadthFirstTraversal[T] = apply( (x: T) => adjlist(x).toList)
}





class DepthFirstTraversal[V]( val nbrs: V => List[V]) extends GraphTraversal[V] {
  def insert(zs: List[E])(ys: E*): List[E] = ys ++: zs

  def clsdNbrs(x: V, open: Set[V]): List[V] = nbrs(x).filter( ! open(_))

  def allPathsFromAvoiding(x: V, open: Set[V]): List[ List[V]] = clsdNbrs(x, open) match {
    case Nil => List(List(x))
    case ys => for{ y <- ys; sy <- allPathsFromAvoiding( y, open + x)} yield (x :: sy)
  }

  def allPathsFrom(x: V): List[List[V]] = allPathsFromAvoiding(x, Set[V]())


   def subPathUpto( path: List[V], x: V): List[V] = {
    val px = path.takeWhile( _ != x)
    if (px.length < path.length) px :+ x  else Nil
  }


  def dftWithSink(x: V): DepthFirstTraversal[V] = 
    DepthFirstTraversal( (z: V) => if (z == x) List[V]() else nbrs(z))
    
  def dftEdgeRmvd(x: V, y: V): DepthFirstTraversal[V] = {
      DepthFirstTraversal( (z: V) => 	z match{ 
                                          case `x` => nbrs(z).filter(_ != y)
                                          case `y` => nbrs(z).filter(_ != x)
                                          case _ => nbrs(z)
                                        }
                        )
  }  

  def allPathsBetween(x: V, y: V): List[List[V]] = {
    val snkd = dftWithSink(y)
    snkd.allPathsFrom(x).map( snkd.subPathUpto(_, y) ).filter( _ != Nil )
  }

}

object DepthFirstTraversal {
  def apply[V](nbrs: V => List[V]): DepthFirstTraversal[V] = new DepthFirstTraversal(nbrs)
  def apply[V](adjlist: Map[V, IndexedSeq[V]]): DepthFirstTraversal[V] = apply( (x: V) => adjlist(x).toList)
}


/**
  * many functions not yet implemented
object GraphTraversalOps{
    def edgeSwap( e: (Int, Int), f: (Int, Int)): ((Int, Int), (Int, Int)) = 
      ( makeLink(e._1, f._1), makeLink(e._2, f._2) )
      
    def areCnctd(al: AL)(x: Int, y: Int): Boolean = DepthFirstTraversal(al).dftWithSink(y).from(x).contains(y)


    def randomLink(al: AL): Option[(Int, Int)] = linksInAdjList(al) match{
      case IndexedSeq() => None
      case links => Some( links( nextInt(links.length) ) )
    }


    def onCycle(al: AL)(xy: (Int, Int)): Boolean = xy match
      { case(x, y) => areCnctd( linksRmvd(al)((x, y)) )(x, y)}

    def firstLinkOnCycle(al: AL): Option[(Int, Int)] = linksInAdjList(al).find( onCycle(al) )

    def randomLinkOnCycle(al: AL): Option[(Int, Int)] = linksOnCycle(al) match{
      case IndexedSeq() => None
      case ls => Some(ls( nextInt(ls.length) ))
    }

    def hasCycle(al: AL): Boolean = linksInAdjList(al).find(onCycle(al)) match{
      case None => false
      case Some(_) => true
    }

    def linksOnCycle(al: AL): IndexedSeq[(Int, Int)] = 
      if (al.isEmpty) IndexedSeq[ (Int, Int) ]() else linksInAdjList(al).filter(onCycle(al))


    

    def numLinksOnCycle(al: AL): Int = linksOnCycle(al).length

  //define a component as a set of Ints
    //write a test to check if the DepthFirstTraversal.from produces a list of distinct elements
    def vtxCmpnt(al: AL)( x: Int): Set[Int] = DepthFirstTraversal(al).from(x).toSet

    def addCmpnt(c: Set[Int], cs: List[Set[Int]], mkd: Set[Int]): List[Set[Int]] = {
      if (c.isEmpty) cs
      else { if (mkd(c.head)) cs else (c :: cs) }
    }

    def components(al: AL): List[Set[Int]] = {
      val dftr = DepthFirstTraversal(al)
      ( (List[Set[Int]](), Set[Int]()) /: vertexes(al)) {
        case ((cs, mkd), x) =>	if (mkd(x)) (cs, mkd) 
                                else{ val c = dftr.from(x).toSet; (c :: cs, mkd ++ c)}
      }._1
    }

    def largestCmpnt(al: AL): Set[Int] = components(al).maxBy( _.size)

      

    def projection(al: AL)(c: Set[Int]): AL = c.map{ case x => (x, al(x).filter( c(_) ) )}.toMap
      
    
}


*/
