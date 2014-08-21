package learn.graph
import scala.language.higherKinds

object Edges {

  trait Edge[V] {
    def reverse: Edge[V]
    def endPoints: Iterable[V]
    override def equals(that: Any): Boolean
    def hashKey: Int
    override def hashCode = hashKey
  }

//an example to show that hyper edges can be played with
  case class HyperBond[V](val endPoints: Set[V]) extends Edge[V] { 
    def reverse: HyperBond[V] = HyperBond[V](endPoints)
    override def equals(that: Any): Boolean = that match {
      case HyperBond(vset) => endPoints == vset
      case _ => false
    }
    def hashKey: Int  = endPoints.hashCode
  }

  case class Bond[V](val head: V, val tail: V) extends Edge[V]{
    def endPoints: Set[V] = Set(head, tail)
    def reverse: Bond[V] = Bond(head, tail)
    override def equals(that: Any): Boolean = that match {
      case Bond(x, y) => ( x == head && y == tail) || ( x == tail && y == head )
      case _ => false
    }
    def hashKey: Int = ( ((head, tail).hashCode + (tail, head).hashCode) )/2
  }
    
  case class Arrow[V](val head: V, val tail: V) extends Edge[V]{
    def reverse: Arrow[V] = Arrow[V](head, tail)
    override def equals(that: Any): Boolean = that match {
      case Arrow(x, y) => head == x && tail == y
      case _ => false
    }
    def endPoints: List[V] = List(head, tail)
    def hashKey: Int = (head, tail).hashCode
  }

  trait EdgeLikePair[V, E[_] <: Edge[_]] {
    def pairAsEdge(x: V, y: V): E[V]
    def edgeEndList(e: E[V]): List[ (V, V) ]
    def edgeEnds(e: E[V]): Set[V] 
  }

  trait BondLikePair[V] extends EdgeLikePair[V, Bond]{
    def pairAsEdge(x: V, y: V): Bond[V] = Bond(x, y)
    def edgeEndList(e: Bond[V]): List[ (V, V)] = e match { case Bond(x, y) => List( (x, y), (y, x) ) }
    def edgeEnds(e: Bond[V]): Set[V] = e match { case Bond(x, y) => Set(x, y) }
  }
  object BondLikePair{
    implicit object BondLikePairOfInts extends BondLikePair[Int]
  }

  trait ArrowLikePair[V] extends EdgeLikePair[V, Arrow]{
    def pairAsEdge(x: V, y: V): Arrow[V] = Arrow(x, y)
    def edgeEndList(e: Arrow[V]): List[ (V, V)] = e match { case Arrow(x, y) => List( (x, y) ) }
    def edgeEnds(e: Arrow[V]): Set[V] = e match { case Arrow(x, y) => Set(x, y) }
  }

  object ArrowLikePair{
    implicit object ArrowLikePairOfInts extends ArrowLikePair[Int]
  }

}
