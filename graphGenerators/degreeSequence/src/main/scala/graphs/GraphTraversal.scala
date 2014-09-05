package learn.graph
import scala.util.Random._
import GraphTypeClass._
import GraphEdge.Endogeneous._

/**
  * traverse a graph of nodes of type V
  * to create a list of edges followed
  * written for UNDIRECTED GRAPHS contained in an adjacency list
  * generalize later
  */
object GraphNavigation{

  trait Traversal[V] {
    type E = (V, V)

    def nbrs(x: V): List[V]
    def insert(zs:  List[(V, V)])(ys: (V, V)*):  List[(V, V)]

    def grow(axs: List[E] , queued: Set[V]): List[E]  = axs match{
      case Nil => Nil
      case (a, x) :: azs => {val ys = nbrs(x).filter ( !queued(_) );
        (a, x) :: grow( insert(azs)(ys.map( (x, _)): _*), queued ++ ys.toSet )
      }
    }
    
    def from( x: V): List[V] = grow( List( (x, x) ), Set(x)).map(_._2)
    def withAncs(x: V): List[E] = grow( List( (x, x) ), Set(x) )
  }



  trait BreadthFirstTraversal[V] extends Traversal[V] {
    def insert(zs: List[E])( ys: E*): List[E] = zs ++ ys


    def shortestPathsFrom(x: V): Map[V, List[V]] = { 
      val revPaths = (Map[V, List[V]](x -> List(x)) /: this.withAncs(x).tail){
                        case (paths, (a, y)) => paths.updated( y, y :: paths(a))
      }
      revPaths.mapValues(_.reverse)
    }
  }

  object BreadthFirstTraversal {
    def apply[T](neighs: T => List[T]): BreadthFirstTraversal[T] = new BreadthFirstTraversal[T]{ def nbrs(x: T) = neighs(x)}
    def apply[T](adjlist: Map[T, IndexedSeq[T]]): BreadthFirstTraversal[T] = apply( (x: T) => adjlist(x).toList)
  }



  trait Path[V] {
    this: DepthFirstTraversal[V] =>

    def allRaysAvoiding(open: Set[V])(x: V): List[ List[V]] = nbrs(x).filter( ! open(_)) match {
      case Nil => List(List(x))
      case ys => ys.flatMap(allRaysAvoiding(open + x)).map(x :: _).toList
    }
    def allRays(x: V): List[List[V]] = allRaysAvoiding(Set[V]())(x)

    def truncated(x: V)(p: List[V]): List[V] = {
      val px = p.takeWhile( _ != x)
      if (px.length < p.length) px :+ x else Nil
    }

    def allPaths(x: V, y: V): List[List[V]] = {
      val snkd = this.withSink(y)
      snkd.allRays(x).map( snkd.truncated(y)).filter( _ != Nil)
    }
  }


  trait DepthFirstTraversal[V] extends Traversal[V] with Path[V]{
    def insert(zs: List[E])(ys: E*): List[E] = ys ++: zs

    def withSink(x: V): DepthFirstTraversal[V] = new DepthFirstTraversal[V] {
      def nbrs(y: V): List[V] = if (y == x) List[V]() else this.nbrs(y)
    }
      
    def withoutEdge(x: V, y: V): DepthFirstTraversal[V] = new DepthFirstTraversal[V] {
      def nbrs(z: V): List[V] = z match {
        case `x` => this.nbrs(z).filter(_ != y)
        case `y` => this.nbrs(z).filter(_ != x)
        case _ => nbrs(z)
      }
    }
  }

  object DepthFirstTraversal {
    def apply[V](neighs: V => List[V]): DepthFirstTraversal[V] = new DepthFirstTraversal[V] { def nbrs(x: V) = neighs(x) }
    def apply[V](adjlist: Map[V, IndexedSeq[V]]): DepthFirstTraversal[V] = apply( (x: V) => adjlist(x).toList)
  }
}


