package learn.graph
import scala.util.Random._

object Graph{
  type AL = Map[Int, IndexedSeq[Int]]
	def emptyAL: Map[Int, IndexedSeq[Int]] = Map[Int, IndexedSeq[Int]]().withDefault( (i: Int) => IndexedSeq[Int] () )
	def emptyAL(vs: Seq[Int]): Map[Int, IndexedSeq[Int]] = vs.map{ (_, IndexedSeq[Int]())}.toMap
  def vertexes(al: AL): List[Int] = al.keys.toList //(al.keys ++ al.values.flatten).toList
	
	def linksERRG(p: Double, N: Int): IndexedSeq[ (Int, Int) ] = 
		for { i <- 0 to N; j <- i + 1 until N; if (nextDouble < p )} yield (i, j)

  def adjListERRG(p: Double, N: Int): AL = adjListLinksAdded(emptyAL(Range(0, N)), linksERRG(p, N))

  def adjListFromLinks( links: IndexedSeq[(Int, Int)]): AL = {
      val adjListEmpty = emptyAL
      (emptyAL /: links){ case (al, (x,y)) => al ++ IndexedSeq(x -> (al(x) :+ y), y -> (al(y) :+ x))}
  }

  def adjListLinksAdded(al: AL, links: IndexedSeq[(Int, Int)]): AL = al ++ adjListFromLinks(links)


	trait GraphTraversal {
		//def nbrs( x: Int): List[Int]
		def nbrs: Int => List[Int]
		def insert(zs: List[Int])(ys: Int*): List[Int]
		def updated(qd: Set[Int], ys: Set[Int]): Set[Int] = qd ++ ys

		def grow(xs: List[Int], qd: Set[Int]): List[Int] = xs match {
			case List() => List()
			case x :: zs => { val ys = nbrs(x).filter( ! qd(_) );
					x :: grow( insert(zs)(ys:_*), updated( qd, ys.toSet))
			}
		}
		def from(x: Int): List[Int] = grow( List(x), Set(x))
	}

	class BreadthFirstTraversal(val nbrs: Int => List[Int]) extends GraphTraversal {
		def insert(zs: List[Int])( ys: Int*): List[Int] = zs ++ ys
		def notPathed(xs: List[Int],  paths: Map[Int, List[Int]]): List[Int] = xs.filter( ! paths.isDefinedAt(_))
		def shortestPathsFrom(x: Int): Map[Int, List[Int]] = {
			def extnd(path: List[Int], ys: List[Int]): Map[Int, List[Int]] = ys.zip(ys.map(path :+ _)).toMap
			def collect(xs: List[Int], paths: Map[Int, List[Int]]): Map[Int, List[Int]] = xs match{
					case List() => paths
					case x :: ys => collect(ys, paths ++ extnd(paths(x), notPathed(nbrs(x), paths).filter(ys.contains(_))))
			}
			collect(this.from(x).tail, Map(x -> List(x)))
		}
	}
	object BreadthFirstTraversal {
    def apply(nbrs: Int => List[Int]): BreadthFirstTraversal = new BreadthFirstTraversal(nbrs)
    def apply(adjlist: AL): BreadthFirstTraversal = apply( (x: Int) => adjlist(x).toList)
  }

  class DepthFirstTraversal( val nbrs: Int => List[Int]) extends GraphTraversal {
    def insert(zs: List[Int])(ys: Int*): List[Int] = ys ++: zs
    def clsdNbrs(x: Int, open: Set[Int]): List[Int] = nbrs(x).filter( ! open(_))
    def allPathsFromAvoiding(x: Int, open: Set[Int]): List[ List[Int]] = clsdNbrs(x, open) match {
      case List() => List(List(x))
      case ys => for{ y <- ys; sy <- allPathsFromAvoiding( y, open + x)} yield (x :: sy)
    }
		def allPathsFrom(x: Int): List[List[Int]] = allPathsFromAvoiding(x, Set[Int]())

    def subPathUpto( path: List[Int], x : Int): Option[List[Int]] = path match{
      case List() => None
      case y :: ys =>	{ if (y == x) Some(x :: List[Int]()) 
                          else subPathUpto(ys, x) match {
													 case None => None
													 case Some(p) => Some( y :: p)
													}
												}
   	}
      
    def allPathsBetween(x: Int, y: Int): List[List[Int]] =  //subPathUpto(allPathsFrom(x).filter(_.contains(y))
      for(p <- allPathsFrom(x); sp <- subPathUpto(p, y)) yield sp

		def allPathsBetween2(x: Int, y: Int): List[List[Int]] = 
			for{ p <- DepthFirstTraversal( (z: Int) => if (z == y) List[Int]() else nbrs(z)).allPathsFrom(x) } yield p
	}
	object DepthFirstTraversal {
    def apply(nbrs: Int => List[Int]): DepthFirstTraversal = new DepthFirstTraversal(nbrs)
    def apply(adjlist: AL): DepthFirstTraversal = apply( (x: Int) => adjlist(x).toList)
  }



  def connected(al: AL)(x: Int, y: Int): Boolean = DepthFirstTraversal(al).from(x).contains(y)
  //def vtxComponent(x: Int, al: AL): List[Int] = DepthFirstTraversal(al).from(x)
  def vtxComponent(x,  dftr: DepthFirstTraversal): List[Int] = dftr.from(x)

  def oncycle(x: Int, y: Int, al: AL): Boolean = 

  def components(al: AL): Set[List[Int]] = {
    val dftr = DepthFirstTraversal(al)
    def addCmpt(x: Int, cs: Set[List[Int]], mkd: Set[Int]): (Set[List[Int]], Set[Int]) = 
      if (mkd(x)) (cs, mkd) else { val c = dftr.from(x); (cs + c, mkd ++ c)}
      //if (mkd(x)) (cs, mkd) else { val c = vtxComponent(x, al); (cs + c, mkd ++ c)}

    ( (Set[List[Int]](), Set[Int]()) /: vertexes(al)){ case ((cs, mkd), x) => addCmpt(x, cs, mkd)}._1
  }

    
}
