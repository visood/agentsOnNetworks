package learn.graph
import scala.util.Random._





object Graph{


  /**
    * Start by defining edges
    */
  trait EdgeLike[V] {
    def head: V
    def tail: V
    def reverse: EdgeLike[V]
    def headTails: List[ (V, V) ]
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

