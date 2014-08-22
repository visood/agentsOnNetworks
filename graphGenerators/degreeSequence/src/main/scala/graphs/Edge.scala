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
    def edgeEndPairs(e: E[V]): List[ (V, V) ] //deserves a better, more descriptive name
    def edgeEnds(e: E[V]): (V, V)
  }

  trait BondLikePair[V] extends EdgeLikePair[V, Bond]{
    def pairAsEdge(x: V, y: V): Bond[V] = Bond(x, y)
    def edgeEndsList(e: Bond[V]): List[ (V, V)] = e match { case Bond(x, y) => List( (x, y), (y, x) ) }
    def edgeEnds(e: Bond[V]): (V, V) = e match { case Bond(x, y) => (x, y) }
  }
  object BondLikePair{
    def apply[V]: BondLikePair[V] = new BondLikePair[V]{}
    implicit object BondLikePairOfInts extends BondLikePair[Int]
  }

  trait ArrowLikePair[V] extends EdgeLikePair[V, Arrow]{
    def pairAsEdge(x: V, y: V): Arrow[V] = Arrow(x, y)
    def edgeEndsList(e: Arrow[V]): List[ (V, V)] = e match { case Arrow(x, y) => List( (x, y) ) }
    def edgeEnds(e: Arrow[V]): (V, V) = e match { case Arrow(x, y) => (x, y) }
  }

  object ArrowLikePair{
    def apply[V]: ArrowLikePair[V] = new ArrowLikePair[V]{}
    implicit object ArrowLikePairOfInts extends ArrowLikePair[Int]
  }

}
