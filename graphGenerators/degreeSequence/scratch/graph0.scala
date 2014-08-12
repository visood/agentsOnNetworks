package learn.graph
import scala.util.Random._

object Graph{

  type AL = Map[Int, IndexedSeq[Int]]
  def emptyAL = Map[Int, IndexedSeq[Int]]().withDefault( (i: Int)=>IndexedSeq[Int]())

  def randomGraphER(p: Double, N: Int): IndexedSeq[(Int, Int)] = 
    for{i <- 0 to N; j <- i + 1 until N; if (nextDouble < p)} yield (i, j)

  trait GraphTraversal{
    def nbrs(x: Int): Stream[Int]
    def insert(zs: Stream[Int], ys: Int*): Stream[Int]
    def updated(qd: Set[Int], ys: Set[Int]): Set[Int]
 //   def updated( qd: Set[Int], ys: Seq[Int]): Set[Int] = qd ++ ys

    def grow(xs: Stream[Int], qd: Set[Int]): Stream[Int] = xs match {
      case Stream() => Stream()
      case x #:: zs => {val ys = nbrs(z).filter( ! qd(_) ); x #:: grow( insert(zs, ys:_*), updated( qd, ys))}
    }
		def from(x: Int): Stream[Int] = grow(Stream(x), Set(x))
		
		object GraphTraversal{
			def apply(ns: Int => Seq[Int], 
								ins: (Seq[Int], Stream[Int]) => Stream[Int]  
								upd: (Set[Int], Set[Int]) => Set[Int]
								): GraphTraversal = {
								new GraphTraversal{
									def nbrs(x: Int): Seq[Int] = ns(x)
									def insert(ys: Seq[Int], zs: Stream[Int]): Stream[Int] = ins(ys, zs)
									def updated( qd: Set[Int], ys: Set[Int]): Set[Int] = upd(qd, ys)
								}
			}
		}
  }

  def defaultGTupdate = ((qd, ys): (Set[Int], Set[Int])) => qd ++ ys

  class BreadthFirstTraversal(val nbrs: Int => Seq[Int], val updated: (Set[Int], Set[Int]) => Set[Int] ) extends GraphTraversal {
    def insert(zs: Stream[Int], ys: Int*): Stream[Int] = zs ++ ys
    def upto(sink: Int): BreadthFirstTraversal = {
      new BreadthFirstTraversal((x: Int) => if (x == sink) Seq[Int]() else nbrs(x), 
                              (qd: Set[Int], ys: Set[Int]) => qd + ys.filter( _ != filter))
    }

    def shortestPathsFrom(x: Int): Map[Int, Stream[Int]] = {
      def collectPaths(paths: Map[Int,  Stream[Int]], xs: Stream[Int]): Map[Int, Stream[Int]] = xs match{
        case Stream() => paths
        case x #:: ys => collectPaths(
                            paths ++ (for{ y <- ys; if nbrs(x).contains(y) && ( ! paths.isDefinedAt(y))} yield paths(x) + y),
                            ys
                         )
      }
      val expSeq: Stream[Int] = this.from(x)
      collectPaths(Map( x -> Stream(x)), expSeq.tail)
    }
  }
  object BreadthFirstTraversal {
    def apply(nbrs: Int => Seq[Int]): BreadthFirstTraversal = new BreadthFirstTraversal(nbrs, defaultGTupdate )
    def apply(adjlist: AL): BreadthFirstTraversal = apply( (x: Int) => adjlist(x))
  }

  class DepthFirstTraversal(val nbrs: Int => Seq[Int], val updated: (Set[Int], Set[Int]) => Set[Int] ) extends GraphTraversal {
    def insert(zs: Stream[Int], ys: Int*): Stream[Int] = ys ++: zs
    def upto(sink: Int): DepthFirstTraversal = {
      new DepthFirstTraversal((x: Int) => if (x == sink) Seq[Int]() else nbrs(x), 
                              (qd: Set[Int], ys: Set[Int]) => qd + ys.filter( _ != filter))
    }
    def allPathsFrom(al: AL)(x: Int, open: Set[Int]): Stream[ Stream[Int]] = nbrs(al: AL)(x).filter( ! open(_)) match{
      case Stream() => Stream(Stream(x))
      case ys => for{ y <- ys; sy <- allPathsFrom(al: AL)( y, open + x)} yield (x #:: sy)
    }

		def between(al: AL)(x: Int, y: Int): Stream[ Stream[Int]] = 
		  allPathsFrom(al)(x, Set[Int]()).filter( _.contains(y))


		def connected(al: AL)(x: Int, y: Int): Boolean = allPathsFrom(al)(x, Set[Int]()).find( _.contains(y)) match{
		  case None => false
			case Some(p) => true
	  }

		def onCycle(al: AL)(x: Int, y: Int) = //assuming a link between x and y
		  connected(al ++ Seq(x -> al(x).filter(_ != y), y -> al(y).filter( _ != x)))(x, y)

  }
  object DepthFirstTraversal {
    def apply(nbrs: Int => Seq[Int]): DepthFirstTraversal = new DepthFirstTraversal(nbrs, defaultGTupdate)
    def apply(adjlist: AL): DepthFirstTraversal = apply( (x: Int) => adjlist(x))
  }


  class RandomOrderTraversal(val nbrs: Int => Seq[Int], val updated: (Set[Int], Set[Int]) => Set[Int] ){
    def insert(zs: Stream[Int], ys: Int*): Stream[Int] = {
      val (s0, s1) = shuffle(ys).splitAt( nextInt(ys.length))
      (s0 ++: zs) ++ s1
    }
    def upto(sink: Int): BreadthFirstTraversal = {
      new RandomOrderTraversal((x: Int) => if (x == sink) Seq[Int]() else nbrs(x), 
                              (qd: Set[Int], ys: Set[Int]) => qd + ys.filter( _ != filter))
    }
  }
  object RandomOrderTraversal {
    def apply(nbrs: Int => Seq[Int]): RandomOrderTraversal = new RandomOrderTraversal(nbrs, defaultGTupdate)
    def apply(adjlist: AL): RandomOrderTraversal = apply( (x: Int) => adjlist(x))
  }


  //BF search as a class
  case class BreadthFirstStream( nbrs: Int => Seq[Int]) {
    def from(xs: Stream[Int], discovered: Set[Int]): Stream[Int] = xs match {
      case Stream() => Stream()
      case z #:: zs =>  { val ys = nbrs(z).filter( ! discovered(_)); z #:: from( zs ++ ys , discovered ++ ys) }
    }
    def from(x: Int): Stream[Int] = from( Stream(x), Set(x))
  }

  //BF search as a function
  def bfstream(nbrs: Int => Seq[Int]) ( seed: Int) = {
    def from(xs: Stream[Int], discovered: Set[Int]): Stream[Int] = xs match {
      case Stream() => Stream()
      case z #:: zs =>  { val ys = nbrs(z).filter( ! discovered(_)); z #:: from( zs ++ ys , discovered ++ ys) }
    }
    from( Stream(seed), Set(seed))
  }

  case class DepthFirstStream( nbrs: Int => Seq[Int]) {
    def from(xs: Stream[Int], discovered: Set[Int]): Stream[Int] = xs match {
      case Stream() => Stream()
      case z #:: zs =>  { val ys = nbrs(z).filter( ! discovered(_)); z #:: from( ys ++: zs , discovered ++ ys) }
    }
    def from(x: Int): Stream[Int] = from( Stream(x), Set(x))
  }


  def adjListFromLinks( links: IndexedSeq[(Int, Int)]): AL = {
    val adjListEmpty = emptyAL
    links.foldLeft(adjListEmpty){ case ( ls, (x, y)) => ls.updated(x, y +: ls(x)).updated(y, x +: ls(y))}
  }

  def nbrFn(adjList: AL): Int => Seq[Int] = (x: Int) => adjList(x)
  def nodes(adjList: AL): List[Int] = adjList.keys.toList

  def components(adjlist: AL): List[ Set[Int]] = {
    val traversal = DepthFirstTraversal( nbrFn(adjList) )
    def addVtx(v: Int, cs: Set[List[Int]], mkd: Set[Int]) = if (mkd(v)) (cs, mkd) else (traversal.from(v).toList)
    (( ( List[Set[Int]](), Set[Int]()), nodes(adjList) ) /: { case ( (cs, mkd), v) => addVtx(v, cs, mkd)})._1
  }



  def projection(adjlist: AL)(vs: Set[Int]): AL = vs.map( (v: Int) => adjlist(v).filter( vs(_))).toMap

  def onCycle(al: AL)(x: Int): Boolean = DepthFirstTraversal(nbrFn(a)).from(x).tail
  def onCycle(al: AL)(x: Int, y: Int): Boolean = 
    if (! al(x).contains(y)) DepthFirstTraversal(nbrFn(al)).from(x).contains(y)
  
  
  
    


  case class GraphUnionFind(leader: Map[Int, Int1G], followers: Map[Int, Int], numLeaders: Int, oncycle: Set[(Int, Int)]){
    def selfLed(x: Int) = leader(x) == x
    def find( x: Int): Int = if (selfLed(x)) x else find(leader(x)) 

    lazy val clusterLabels: List[Int] = leader.keys.map( find(_)).toList
   
    def nodes: List[Int] = leader.keys.toList

    lazy val clusters: Map[Int, List[Int]] = {
      val cs: Map[Int, List[Int]] = clusterLabels.zip( Seq.fill(numLeaders)(List[Int]() ) ).toMap
      (cs /: nodes){ case (c, x) => { val lx = find(x); c.updated( lx, x :: c(lx))} }
    }

    def trail(x: Int): Stream[Int] = if (selfLed(x)) x #:: Stream[Int]() else x #:: trail(leader(x))
  
    def trailReversed(x: Int): List[Int] = trail(x).reverse.toList

    def union(x: Int, y: Int): GraphUnionFind = {
      if ( followers(find(x)) <= followers(find(y))) {
        val ty = trailReversed(y); val tx = trailReversed(x)
        val newLeaders = ( leader /: (tx ++ ty)){ case (l, v) => l.updated(v, ty.head)}
        if (ty.head != tx.head){
          val newFoll = followers ++ List( tx.head -> 0, ty.head -> (followers.getOrElse(ty.head, 0) + followers(tx.head)) )
          GraphUnionFind( newLeaders, newFoll, numLeaders - 1, oncycle)
        }
        else GraphUnionFind(newLeaders, followers, numLeaders, oncycle)
      }
      else union(y, x)
    }

    def uniteThrough( x: Int, y: Int): GraphUnionFind = 
      if (find(x) != find(y)) union(x, y) else GraphUnionFind( leader, followers, numLeaders, oncycle + ((x,y)))

    def uniteThrough( xy: (Int, Int)): GraphUnionFind = uniteThrough(xy._1, xy._2)

    def uniteThrough( links: Seq[(Int, Int)]): GraphUnionFind = (this /: links) { case (g, l) => g.uniteThrough(l)}
  }
        
}
      



val al1 = adjListFromLinks( randomGraphER(0.5, 10))
val dftr1 = DepthFirstTraversal( al1)
Range(0, 10).map{ dftr1.cnctd( 0, _)}
val al2 = al1.map{ case(x, nx) => (x + 20, nx.map(_ + 20))}
 val al3 = al1 ++ al2 ++ IndexedSeq( 9 -> (al1(9) :+ 10), 10 -> IndexedSeq(9, 11), 11 -> IndexedSeq(10, 12), 12 -> IndexedSeq(11, 13), 13 -> IndexedSeq(12, 14), 14 -> IndexedSeq(13, 15), 15 -> IndexedSeq(14, 16), 16 -> IndexedSeq(15, 17), 17 ->IndexedSeq(16, 18), 18 -> IndexedSeq(17, 19), 19 -> IndexedSeq(18, 20), 20 -> (al2(20) :+ 19))
