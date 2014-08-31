package learn.graph
import scala.util.Random._
import scala.language.higherKinds
import scala.language.postfixOps
import scala.Predef

  /**
    * We want to be able to work with Graphs that can hold more than one type of edge
    * Type of an edge may not be set only by its directionality, but also by its context
    * For example, friendships may be made at work, or at a bar.
    */

object GraphTypeClass{
  import GraphEdge._
  import EdgeTypeClass._

  trait GraphLike[V, G] {

    this: EdgeLike[V] =>

    def vertexes(g: G): Set[V] 

    def degrees(g: G): Map[V, Int] = {
      val al = adjList(g)
      al.map{ case (x, ys) => (x, ys.length) }
    }

    def edges(g: G): Set[Link[V]]

    def adjList(g: G): Map[V, IndexedSeq[V]]

    def from(es: Set[Link[V]]): G

    def from(ts: Set[(V, V)])(implicit dummy: DummyImplicit): G 
      = from( ts.map{case t => asEdge(t._1, t._2)} )

    def from(al: Map[V, IndexedSeq[V]]): G = {
      val ts: Set[(V, V)] = al.toSeq.flatMap{ case (x, ys) => ys.map( (x, _) ) }.toSet
      from(ts)
    }

    def merger[H](g: G, h: H)
      (implicit ev: GraphLike[V, H]): G = { 
      from((edges(g) ++ ev.edges(h)))
    }

    def differ[H](g: G, h: H)
      (implicit ev: GraphLike[V, H]): G = {
      from((edges(g) -- ev.edges(h)))
    }
  }

  
  object GraphLike{

    /**
      * think about what imports to make
      * be guided by : "Do not mention Bond or Arrow "
      * rewrite Edges if necessary
      */
    
      
    type IL[V] = Map[V, IndexedSeq[Link[V] ] ]
    trait IncList[V] extends GraphLike[V, IL[V]] {
      this: EdgeLike[V] =>

      def vertexes(g: IL[V]): Set[V] = g.keys.toSet //assuming a vertex w/o edges is included with an empty Vector

      def adjList(g: IL[V]): Map[V, IndexedSeq[V]] = g.mapValues( _.map(_.ends)).mapValues(_.map(_._2))
      
      def edges(g: IL[V]): Set[Link[V] ] = g.toSeq.flatMap(_._2).toSet

      def from(es: Set[Link[V]]): IL[V] = {
        es.toSeq.flatMap{ case e => e.sources.map( (_, e) )}.groupBy(_._1).mapValues(_.map(_._2).toIndexedSeq)
      }

    }

    object IncList{
      trait Undirected[V] extends IncList[V] with BondLike[V]
      object Undirected {
        object OnInts extends Undirected[Int]
      }

      trait Directed[V] extends IncList[V] with ArrowLike[V]
      object Directed {
        object OnInts extends Directed[Int]
      }

    }

    type AL[V] = Map[V, IndexedSeq[V]]

    trait AdjList[V] extends GraphLike[V, AL[V]] {
      this: EdgeLike[V] =>

      def vertexes(g: AL[V]): Set[V] = g.keys.toSet

      def adjList(g: AL[V]): Map[V, IndexedSeq[V]] = g.asInstanceOf[Map[V, IndexedSeq[V]]]
      def edges(g: AL[V]): Set[Link[V]] = {
        g.toSeq.flatMap{ case (x, ys) => ys.map{ case y => asEdge(x, y)} }.toSet
      }

      def from(es: Set[Link[V]]): AL[V] = {
        es.toSeq.flatMap(_.order ).groupBy(_._1).map{case (x, ys) => (x, ys.map(_._2).toIndexedSeq) }
      }

    }

    object AdjList{


      trait Undirected[V] extends AdjList[V] with BondLike[V] 

      object Undirected {
        object OnInts extends Undirected[Int]
      }

      trait Directed[V] extends AdjList[V] with ArrowLike[V] 

      object Directed {
        object OnInts extends Directed[Int]
      }

    }



    type ESet[V] = Set[Link[V]]

    trait EdgeSet[V] extends GraphLike[V, ESet[V]] {
      this: EdgeLike[V] => 

      def vertexes(g: Set[Link[V]]): Set[V] = g.flatMap(_.ends match { case (x, y) => Set(x, y) } )

      def adjList(g: Set[Link[V]]): Map[V, IndexedSeq[V]] = 
        g.toSeq.flatMap( _.order ).groupBy(_._1).map{ case(x, ys) => (x, ys.map(_._2).toIndexedSeq) }

      def edges(g: Set[Link[V]]): Set[Link[V]] = g

      def from(es: Set[Link[V]]): Set[Link[V]] = es
    }

    object EdgeSet{


      trait Undirected[V] extends EdgeSet[V] with BondLike[V] 

      object Undirected {
        object OnInts extends Undirected[Int] 
      }


      trait Directed[V] extends EdgeSet[V] with ArrowLike[V]

      object Directed {
        object OnInts extends Directed[Int] 
      }

    }
  }
}




