package learn.graph
import scala.util.Random._

object Graph{
  type AL = Map[Int, IndexedSeq[Int]]
	def emptyAL = Map[Int, IndexedSeq[Int]]().withDefault( (i: Int) => IndexedSeq[Int] () )
	
	def randomGraphER(p: Double, N: Int): IndexedSeq[ (Int, Int) ] = 
		for { i <- 0 to N; j <- i + 1 until N; if (nextDouble < p )} yield (i, j)

		trait GraphTraversal {
			def nbrs( x: Int): Stream[Int]
			def insert(zs: Stream[Int], ys: Int*): Stream[Int]
			def updated(qd: Set[Int], ys: Set[Int]): Set[Int]

			def grow(xs: Stream[Int], qd: Set[Int]): Stream[Int] = xs match {
				case Stream() => Stream()
				case x #:: zs => { val ys = nbrs(z).filter( ! qd(_) ); x #:: grow( insert(zs, ys:_*), updated( qd, ys))}
			}
			def from(x: Int): Stream[Int] = grow( Stream(x), Set(x))

			object GraphTraversal{
