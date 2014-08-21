package learn.graph
import scala.util.Random._




/**
  * Using type classes
  */



  /**
    * Start by defining edges
    * We want to use ordinary tuples (V, V) for edges
    * At the same time we want to be able to distinguish cases
    * where this (V, V) is a directed or undirected edge
    */
object Edges {

  trait Edge[V] {
    def reverse: Edge[V]
    def endPoints: Iterable[V]
    override def equals(that: Any): Boolean
    def hashKey: Int
    override def hashCode = hashKey
  }

  case class HyperBond[V](val endPoints: Set[V]) { //an example to show that hyper edges can be played with
    def reverse: HyperBond[V] = HyperBond[V](endPoints)
    override def equals(that: Any): Boolean = that match {
      case HyperBond(vset) => endPoints == vset
      case _ => false
    }
    def hashKey: Int  = endPoints.hashCode
  }

  case class Bond[V](val head: V, val tail: V){
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

  implicit object tupleToBond{
    def apply[V](x: V, y: V): Bond[V] = Bond(x, y)
    def apply[V](xy: (V, V)): Bond[V] = Bond(xy._1, xy._2)
  }

  implicit object tupleToArrow{
    def apply[V](x: V, y: V): Arrow[V] = Arrow(x, y)
    def apply[V](xy: (V, V)): Arrow[V] = Arrow(xy._1, xy._2)
  }

}


object Graph{
  import scala.language.higherKinds
  type AL[V] = Map[V, IndexedSeq[V]]
  type ESet[V] = Set[(V, V)]

  //trait GraphLike[V, G] {
  trait GraphLike[V, G[_]]{

    def vertexes(g: G[V]): IndexedSeq[V] 
    def vertexSet(g: G[V]): Set[V]
    def edgeSet(g: G[V]): Set[(V, V)]
    def adjList(g: G[V]): AL[V]
    def from(es: ESet[V]): G[V]
    def degrees(g: G[V]): Map[V, Int] = {
      val al = adjList(g)
      al.map{ case (x, ys) => (x, ys.length) }
    }
    def merger[H[_]](g: G[V], h: H[V])(implicit ev: GraphLike[V, H]): G[V] = from( edgeSet(g) ++ ev.edgeSet(h))

    def differ[H[_]](g: G[V], h: H[V])(implicit ev: GraphLike[V, H]): G[V] = from (edgeSet(g) -- ev.edgeSet(h))
  }

  
  object AdjList{
    trait GraphLikeAdjList[V] extends GraphLike[V, AL] {
      def vertexes(g: AL[V]) = g.keys.toIndexedSeq
      def vertexSet(g: AL[V]) = g.keys.toSet
      def adjList(g: AL[V]): AL[V] = g
      def edgeSet(g: AL[V]): Set[ (V, V) ] = g.toSeq.flatMap{ case (x, ys) => ys.map((x, _))}.toSet
      def from(es: ESet[V]): AL[V] = {
        val lfb: ESet[V] = es.flatMap{ case (x, y) => Set( (x, y), (y, x) )}
        lfb.groupBy(_._1).map{ case (x, ys) => (x, ys.map(_._2).toIndexedSeq) }
      }
    }
    //object GraphLikeAdjList{
      //implicit object GraphLikeAdjListOnInts extends GraphLikeAdjList[Int]
    //}
  }

  object EdgeSet{
    trait GraphLikeEdgeSet[V] extends GraphLike[V, ESet] {
      def vertexes(g: ESet[V]) = vertexSet(g).toIndexedSeq
      def vertexSet(g: ESet[V]) = g.flatMap{ case (x, y) => List(x, y)}
      def adjList(g: ESet[V]): AL[V] = {
        val lfb: ESet[V] = g.flatMap{ case (x, y) => Set( (x, y), (y, x) )}
        lfb.groupBy(_._1).map{ case (x, ys) => (x, ys.map(_._2).toIndexedSeq) }
      }
      def edgeSet(g: ESet[V]): Set[ (V, V) ] = g
      def from(es: ESet[V]): ESet[V] = es
    }
    //object GraphLikeEdgeSet{
      //implicit object GraphLikeEdgeSetOnInts extends GraphLikeEdgeSet[Int]
    //}
  }
}


object GraphOps{
  import scala.language.higherKinds
  import Graph.GraphLike
  import Graph.AdjList.GraphLikeAdjList
  import Graph.EdgeSet.GraphLikeEdgeSet
  implicit object GraphLikeAdjListOnInts extends GraphLikeAdjList[Int]
  implicit object GraphLikeEdgeSetOnInts extends GraphLikeEdgeSet[Int]
  def edgeSet[V, G[_]] (g: G[V])(implicit evg: GraphLike[V, G]): ESet[V] = evg.edgeSet(g)

  def vertexSet[V, G[_]](g: G[V])(implicit evg: GraphLike[V, G]): Set[V] = evg.vertexSet(g)
  def vertexes[V, G[_]](g: G[V])(implicit evg: GraphLike[V, G]): IndexedSeq[V] = evg.vertexes(g)

  def adjList[V, G[_]](g: G[V])(implicit evg: GraphLike[V, G]): AL[V] = evg.adjList(g)

  def degrees[V, G[_]](g: G[V])(implicit evg: GraphLike[V, G]): Map[V, Int] = evg.degrees(g)

  def merger[V, G[_], H[_]](g: G[V], h: H[V])(implicit evg: GraphLike[V, G], evh: GraphLike[V, H]): G[V] = {
    evg.merger( g, h)
  }
  def differ[V, G[_], H[_]](g: G[V], h: H[V])(implicit evg: GraphLike[V, G], evh: GraphLike[V, H]): G[V] = {
    evg.differ( g, h)
  }

  /**
    * Some tests
    * Try to implement using ScalaTest in the directory test
    */
  val al: AL[Int] = Map( 1 -> IndexedSeq(2,3), 2 -> IndexedSeq(1), 3 -> IndexedSeq(1) )
  val es: ESet[Int] = Set( (2,3), (1,2) )
  def vertexes1 = vertexes(al)
  def vertexes2 = vertexes(es)
  def vertexSet1 = vertexSet(al)
  def vertexSet2 = vertexSet(es)
  def degrees1 = degrees(al)
  def degrees2 = degrees(es)
  def adjList1 = adjList(al)
  def adjList2 = adjList(es)
  def edgeSet1 = edgeSet(al)
  def edgeSet2 = edgeSet(es)
  def tryToMerge1 = merger(al, es)
  def tryToMerge2 = merger(es, al)
  def tryToDiffer1 = differ(al, es)
  def tryToDiffer2 = differ(es, al)
}
    









    







  case class Arrow[V](val head: V, val tail: V) extends EdgeLike[V]{
    def reverse = Arrow(tail, head)
    def headTails: List[ (V, V) ] = List( (head, tail) )
  }

  case class Edge[V](val head: V, val tail: V) extends EdgeLike[V]{
    override def equals(that: Any) = that match {
      case Edge(u, v) => ( (u == head) && (v == tail) ) || ( (u == tail) && (v == head) )
      case Arrow(u, v) => ( (u == head) && (v == tail) ) || ( (u == tail) && (v == head) )
      case _ => false
    }
    override def hashCode = (head, tail).hashCode + (tail, head).hashCode

    def reverse = Edge(tail, head)

    def headTails: List[ (V, V) ] = List( (head, tail), (tail, head) )
  }


  trait AbstractGraph[V, E <: EdgeLike[V]]{
    /**
      * V is the type of graph vertexes
      * We define Graph as a trait.
      * Some of its features will be left unimplemented, 
      * to be filled in by an extending class such as a erdos-renyi random graph.
      * Operations common to all graphs will be implemented here.
      * A graph is defined by a set of its edges, which could be directed (Arrow)
      * or undirected (Edge)
      * 
      */

    type Self <: AbstractGraph[V, E]
    
    type AL = Map[V, IndexedSeq[V]] //adjacency list

    def edgeSet: Set[E]

    def connect(x: V, y: V): E 

    def headTails(e: E): List[(V, V)] 

    def apply(es: Set[E]): Self 

    val adjList: AL = edgeSet.flatMap(headTails).groupBy(_._1).mapValues(_.map(_._2).toIndexedSeq)

    val vertexes: Seq[V] = (adjList.values.flatten.toIndexedSeq ++ adjList.keys).distinct

    def degrees: Map[V, Int] = adjList.map{ case(x, nx) => (x, nx.length)}
    /**
      * composed graphs
      */
    def ++ (links: Set[E]): Self = apply( this.edgeSet ++ links )

    def ++ [G <: AbstractGraph[V,E]](that: G): Self = apply( this.edgeSet ++ that.edgeSet)
        
    def -- (links: Set[E]): Self = apply( this.edgeSet -- links) //a better operator to remove edges?

    def --  [G <: AbstractGraph[V,E]](that: G): Self =  this -- that.edgeSet

  }


  class Graph[V](val edgeSet: Set[Edge[V]]) extends AbstractGraph[V, Edge[V]]{
    type Self = Graph[V]
    def headTails(e: Edge[V]): List[(V, V)] = e.headTails
    def connect(x: V, y: V): Edge[V] = Edge(x, y)
    def apply(es: Set[ Edge[V] ]): Graph[V] = new Graph[V](es)
  }

  object Graph{
    def apply[V](ls: (V, V)*): Graph[V] =  new Graph[V]( ls.map{ case (x, y) => Edge(x, y)}.toSet ) 
    def apply[V](al: Map[V, IndexedSeq[V]]): Graph[V] = new Graph[V] (al.flatMap{ case (x, ys) => ys.map(Edge(x, _))}.toSet)
  }

    

  object Graph {
    /**
      * functions that can work on a graph, or graph like object
      */
    /*
    def apply[V](links: (V, V)*): Graph[V] = new Graph[V]{
      val adjList = asAdjList( links.map{ case(x, y) => E(x, y) }: _* )
    }
    */
    def apply[V](links: E[V]*): Graph[V] = new Graph[V]{
      val adjList = asAdjList(links: _*)
    }
  }
  /**
    * links to an adjacency list, assuming links are undirected,
    * and only single copies provided 
    */
  def asAdjList[V](links: E[V]*): AL[V] = { 
    val lfb: Seq[(V, V)] = links.map{ case E(x, y) => (x, y)} ++ links.map{ case E(x,y) => (y, x)}
    lfb.groupBy(_._1).mapValues(_.map(_._2).toIndexedSeq)
  }

  def vertexes[V](links: Seq[E[V]]): Seq[V] = 
    links.map{ case E(x, y) => (x, y)}.unzip match { case (xs, ys) => (xs ++ ys).distinct} 


  def emptyAL[V](vs: Seq[V] = IndexedSeq[V]()): AL[V] = ( (for(v <- vs) yield(v, IndexedSeq[V]())) ).toMap 

  def emptyGraph[V](vs: Seq[V]): Graph[V] = new Graph[V]{
    val adjList = emptyAL(vs)
  }

  def edgesInAdjList[V](al: AL[V]): Set[E[V]] = 
    al.flatMap{ case (x, ys) => ys.map(E(x, _))}.toSet

}

