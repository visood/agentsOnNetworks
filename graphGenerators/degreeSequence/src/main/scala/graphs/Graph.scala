package learn.graph
import scala.util.Random._
import scala.language.higherKinds
import scala.language.postfixOps
import scala.Predef



/**
  * Using type classes
  */



  /**
    * Start by defining edges
    * We want to use ordinary tuples (V, V) for edges
    * At the same time we want to be able to distinguish cases
    * where this (V, V) is a directed or undirected edge
    */



object GraphTypeClass{
  import Edges._
  type AL[V, E[_] <: Edge[_]] = Map[V, IndexedSeq[V]]
  type ESet[V, E[_] <: Edge[_] ] = Set[E[V]]
  

  //trait GraphLike[V, G] {
  trait GraphLike[V, E[_] <: Edge[_], G[_,_[_]]]{
    def elp: EdgeLikePair[V, E]
    def vertexes(g: G[V, E]): IndexedSeq[V] 
    def vertexSet(g: G[V, E]): Set[V]

    def degrees(g: G[V, E]): Map[V, Int] = {
      val al = adjList(g)
      al.map{ case (x, ys) => (x, ys.length) }
    }

    def edgeSet(g: G[V, E]): Set[E[V]]
    def edgeEnds(g: G[V, E]): List[(V, V)] = edgeSet(g).toList.map(elp.edgeEnds)

    def adjList(g: G[V, E]): Map[V, IndexedSeq[V]]

    def from(es: Set[E[V]]): G[V, E]
    def from(ts: Set[(V, V)])(implicit d: DummyImplicit): G[V, E] = from( ts.map{ case t => (elp.pairAsEdge _).tupled(t) } )
    def from(al: Map[V, IndexedSeq[V]]): G[V, E] = {
      val ts: Set[(V, V)] = al.toSeq.flatMap{ case (x, ys) => ys.map( (x, _) ) }.toSet
      from(ts)
    }

    def merger[H[_, _[_]]](g: G[V, E], h: H[V, E])(implicit ev: GraphLike[V, E, H]): G[V, E] = { 
      from((edgeSet(g) ++ ev.edgeSet(h)))
    }

    def differ[H[_, _[_]]](g: G[V, E], h: H[V, E])(implicit ev: GraphLike[V, E, H]): G[V, E] = {
      from((edgeSet(g) -- ev.edgeSet(h)))
    }
  }

  
  object GraphLike{

    /**
      * think about what imports to make
      * be guided by : "Do not mention Bond or Arrow "
      * rewrite Edges if necessary
      */
    import Edges.Edge
    
      
    //def withEdgeType[V, E[_] <: Edge[_], G[_, _[_]]](elp: EdgeLikePair[V, E]): GraphLike[V, E, G] = {
      

    trait AdjList[V, E[_] <: Edge[_]] extends GraphLike[V, E, AL] {
      def vertexes(g: AL[V, E]) = g.keys.toIndexedSeq
      def vertexSet(g: AL[V, E]) = g.keys.toSet
      def adjList(g: AL[V, E]): Map[V, IndexedSeq[V]] = g.asInstanceOf[Map[V, IndexedSeq[V]]]
      def edgeSet(g: AL[V, E]): Set[E[V]] = {
        g.flatMap{ case (x, ys) => ys.map{ case y => elp.pairAsEdge(x, y)} }.toSet
      }

      def from(es: Set[E[V]]): AL[V, E] = {
        es.toSeq.flatMap( elp.edgeEndsPairs ).groupBy(_._1).map{case (x, ys) => (x, ys.map(_._2).toIndexedSeq) }
      }

    }

    object AdjList{
      import Edges.BondLikePair
      import Edges.ArrowLikePair


      object Undirected {
        def apply[V]: GraphLike[V, Bond, AL] = (new AdjList[V, Bond]{
            val elp: EdgeLikePair[V, Bond] = BondLikePair[V] 
          }).asInstanceOf[GraphLike[V, Bond, AL]]
      }

      object Directed {
        def apply[V]: GraphLike[V, Arrow, AL] = (new AdjList[V, Arrow]{
            val elp: EdgeLikePair[V, Arrow] = ArrowLikePair[V] 
          }).asInstanceOf[GraphLike[V, Arrow, AL]]
      }

    }



    trait EdgeSet[V, E[_] <: Edge[_]] extends GraphLike[V, E, ESet] {
      def vertexSet(g: Set[E[V]]): Set[V] = g.flatMap{ 
        case e =>  elp.edgeEnds(e) match { case (x, y) => Set(x,y)}}
      def vertexes(g: Set[E[V]]): IndexedSeq[V] = vertexSet(g).toIndexedSeq

      def adjList(g: Set[E[V]]): Map[V, IndexedSeq[V]] = {
        g.toSeq.flatMap( elp.edgeEndsPairs ).groupBy(_._1).map{ case(x, ys) => (x, ys.map(_._2).toIndexedSeq) }
      }

      def edgeSet(g: Set[E[V]]): Set[E[V]] = g

      def from(es: Set[E[V]]): Set[E[V]] = es
    }

    object EdgeSet{
      import Edges.BondLikePair
      import Edges.ArrowLikePair

      object Undirected {
        def apply[V]: GraphLike[V, Bond, ESet] = (new EdgeSet[V, Bond]{
            val elp: EdgeLikePair[V, Bond] = BondLikePair[V] 
          }).asInstanceOf[GraphLike[V, Bond, ESet]]
      }

      object Directed {
        def apply[V]: GraphLike[V, Arrow, ESet] = (new EdgeSet[V, Arrow]{
            val elp: EdgeLikePair[V, Arrow] = ArrowLikePair[V] 
          }).asInstanceOf[GraphLike[V, Arrow, ESet]]
      }

    }

  }
}




