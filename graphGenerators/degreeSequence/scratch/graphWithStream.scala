package learn.graph
import scala.util.Random._

object Graph{
  type AL = Map[Int, IndexedSeq[Int]]
	def emptyAL = Map[Int, IndexedSeq[Int]]().withDefault( (i: Int) => IndexedSeq[Int] () )
	
	def randomGraphER(p: Double, N: Int): IndexedSeq[ (Int, Int) ] = 
		for { i <- 0 to N; j <- i + 1 until N; if (nextDouble < p )} yield (i, j)

  def adjListFromLinks( links: IndexedSeq[(Int, Int)]): AL = {
      val adjListEmpty = emptyAL
      links.foldLeft(adjListEmpty){ case ( ls, (x, y)) => ls.updated(x, y +: ls(x)).updated(y, x +: ls(y))}
  }


	trait GraphTraversal {
		//def nbrs( x: Int): Stream[Int]
		def nbrs: Int => Stream[Int]
		def insert(zs: Stream[Int])(ys: Int*): Stream[Int]
		def updated(qd: Set[Int], ys: Set[Int]): Set[Int] = qd ++ ys

		def grow(xs: Stream[Int], qd: Set[Int]): Stream[Int] = xs match {
			case Stream() => Stream()
			case x #:: zs => { val ys = nbrs(x).filter( ! qd(_) );
					x #:: grow( insert(zs)(ys:_*), updated( qd, ys.toSet))
			}
		}
		def from(x: Int): Stream[Int] = grow( Stream(x), Set(x))
	}

	class BreadthFirstTraversal(val nbrs: Int => Stream[Int]) extends GraphTraversal {
		def insert(zs: Stream[Int])( ys: Int*): Stream[Int] = zs ++ ys
		def notPathed(xs: Stream[Int],  paths: Map[Int, Stream[Int]]): Stream[Int] = xs.filter( ! paths.isDefinedAt(_))
		def shortestPathsFrom(x: Int): Map[Int, Stream[Int]] = {
			def extnd(path: Stream[Int], ys: Stream[Int]): Map[Int, Stream[Int]] = ys.zip(ys.map(path :+ _)).toMap
			def collect(xs: Stream[Int], paths: Map[Int, Stream[Int]]): Map[Int, Stream[Int]] = xs match{
					case Stream() => paths
					case x #:: ys => collect(ys, paths ++ extnd(paths(x), notPathed(nbrs(x), paths).filter(ys.contains(_))))
			}
			collect(this.from(x).tail, Map(x -> Stream(x)))
		}
	}
	object BreadthFirstTraversal {
    def apply(nbrs: Int => Stream[Int]): BreadthFirstTraversal = new BreadthFirstTraversal(nbrs)
    def apply(adjlist: AL): BreadthFirstTraversal = apply( (x: Int) => adjlist(x).toStream)
  }

  class DepthFirstTraversal( val nbrs: Int => Stream[Int]) extends GraphTraversal {
    def insert(zs: Stream[Int])(ys: Int*): Stream[Int] = ys ++: zs
    def clsdNbrs(x: Int, open: Set[Int]): Stream[Int] = nbrs(x).filter( ! open(_))
    def allPathsFromAvoiding(x: Int, open: Set[Int]): Stream[ Stream[Int]] = clsdNbrs(x, open) match {
      case Stream() => Stream(Stream(x))
      case ys => for{ y <- ys; sy <- allPathsFromAvoiding( y, open + x)} yield (x #:: sy)
    }
		def allPathsFrom(x: Int): Stream[Stream[Int]] = allPathsFromAvoiding(x, Set[Int]())

    def subPathUpto( path: Stream[Int], x : Int): Option[Stream[Int]] = path match{
      case Stream() => None
      case y #:: ys =>	{ if (y == x) Some(x #:: Stream[Int]()) 
                          else subPathUpto(ys, x) match {
													 case None => None
													 case Some(p) => Some( y #:: p)
													}
												}
   	}
      
    def allPathsBetween(x: Int, y: Int): Stream[Stream[Int]] =  //subPathUpto(allPathsFrom(x).filter(_.contains(y))
      for(p <- allPathsFrom(x); sp <- subPathUpto(p, y)) yield sp

		def allPathsBetween2(x: Int, y: Int): Stream[Stream[Int]] = 
			for{ p <- DepthFirstTraversal( (z: Int) => if (z == y) Stream[Int]() else nbrs(z)).allPathsFrom(x) } yield p
			//for(p <- DepthFirstTraversal( al ++ Seq(y -> Stream())).allPathsFrom(x)) yield p
      
	}
	object DepthFirstTraversal {
    def apply(nbrs: Int => Stream[Int]): DepthFirstTraversal = new DepthFirstTraversal(nbrs)
    def apply(adjlist: AL): DepthFirstTraversal = apply( (x: Int) => adjlist(x).toStream)
  }

}
    
    


                              


    


