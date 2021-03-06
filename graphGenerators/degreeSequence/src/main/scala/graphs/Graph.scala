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
  import GraphEdge.Endogeneous._
  import GraphEdge.Endogeneous.Link._
  type IQ[T] = IndexedSeq[T]

  trait GraphLike[V, G] {

    def numVertexes(g: G): Int

    def numEdges(g: G): Int

    def vertexes(g: G): Set[V] 

    def degrees(g: G)(implicit eve: Link.TypeClass[V]): Map[V, Int] = {
      val al = adjList(g)
      al.map{ case (x, ys) => (x, ys.length) }
    }

    def edges(g: G)(implicit evE: Link.TypeClass[V]): Set[Link[V]]

    def adjList(g: G): Map[V, IQ[V]]

    def from(es: Set[Link[V]]): G

    def from(ts: Set[(V, V)])(implicit evE: Link.TypeClass[V]): G 
      = from( ts.map{case t => evE.asEdge(t._1, t._2)} )

    def from(al: Map[V, IQ[V]])(implicit evE: Link.TypeClass[V]): G = {
      val ts: Set[(V, V)] = al.toSeq.flatMap{ case (x, ys) => ys.map( (x, _) ) }.toSet
      from(ts)
    }


    def merger[H](g: G, h: H)(implicit ev: GraphLike[V, H], evE: Link.TypeClass[V]): G 
      = from((edges(g) ++ ev.edges(h)))

    def differ[H](g: G, h: H)(implicit ev: GraphLike[V, H], evE: Link.TypeClass[V]): G 
      = from((edges(g) -- ev.edges(h)))
      
  }

  
  object GraphLike{

    /**
      * think about what imports to make
      * be guided by : "Do not mention Bond or Arrow "
      * rewrite Edges if necessary
      */
    
      
    type IL[V] = Map[V, IQ[Link[V] ] ]
    trait IncList[V] extends GraphLike[V, IL[V]] {

      def numVertexes(g: IL[V]): Int = g.keys.size 

      def numEdges(g: IL[V]): Int = g.values.flatten.size //considering each edge as directed

      def vertexes(g: IL[V]): Set[V] = g.keys.toSet //assuming a vertex w/o edges is included with an empty Vector

      def adjList(g: IL[V]): Map[V, IQ[V]] = g.mapValues( _.map(_.ends)).mapValues(_.map(_._2))
      
      def edges(g: IL[V])(implicit evE: Link.TypeClass[V]): Set[Link[V] ] = g.toSeq.flatMap(_._2).toSet

      def from(es: Set[Link[V]]): IL[V] = {
        es.toSeq.flatMap{ case e => e.sources.map( (_, e) )}.groupBy(_._1).mapValues(_.map(_._2).toIndexedSeq)
      }

    }

    object IncList{
      trait Undirected[V] extends IncList[V] 
      /*
      object Undirected {
        object OnInts extends Undirected[Int]
      }
      */

      trait Directed[V] extends IncList[V] 
      /*
      object Directed {
        object OnInts extends Directed[Int]
      }
      */

    }

    type AL[V] = Map[V, IQ[V]]

    trait AdjList[V] extends GraphLike[V, AL[V]] {
      def numVertexes(g: AL[V]): Int = g.keys.size //may be wrong if edges directed, assumes even 0 degree in keys

      def numEdges(g: AL[V]): Int = g.values.map(_.length).sum/2 //assumes undirected edges

      def meanDegree(g: AL[V]): Double = numEdges(g).toDouble/numVertexes(g).toDouble

      def edgeExcess(g: AL[V]): Int = numEdges(g) - numVertexes(g) + 1
      
      def vertexes(g: AL[V]): Set[V] = g.keys.toSet

      def adjList(g: AL[V]): Map[V, IQ[V]] = {
        g.asInstanceOf[Map[V, IQ[V]]]
      }
      def edges(g: AL[V])(implicit evE: Link.TypeClass[V]): Set[Link[V]] = {
        g.toSeq.flatMap{ case (x, ys) => ys.map{ case y => evE.asEdge(x, y)} }.toSet
      }

      def from(es: Set[Link[V]]): AL[V] = {
        es.toSeq.flatMap(_.order ).groupBy(_._1).map{case (x, ys) => (x, ys.map(_._2).toIndexedSeq) }
      }
    }

    object AdjList{


      trait Undirected[V] extends AdjList[V] // what makes this undirected

      object Undirected {
        object OnInts extends Undirected[Int]
      }

      trait Directed[V] extends AdjList[V] //what makes this directed

      object Directed {
        object OnInts extends Directed[Int]
      }

    }



    type ESet[V] = Set[Link[V]]

    trait EdgeSet[V] extends GraphLike[V, ESet[V]] {

      def vertexes(g: Set[Link[V]]): Set[V] = g.flatMap(_.ends match { case (x, y) => Set(x, y) } )

      def adjList(g: Set[Link[V]]): Map[V, IQ[V]] = 
        g.toSeq.flatMap( _.order ).groupBy(_._1).map{ case(x, ys) => (x, ys.map(_._2).toIndexedSeq) }

      def edges(g: Set[Link[V]])(implicit eve: Link.TypeClass[V]): Set[Link[V]] = g

      def from(es: Set[Link[V]]): Set[Link[V]] = es
    }

    object EdgeSet{
      

      trait Undirected[V] extends EdgeSet[V] 

      implicit object  evb extends Bond.TypeClass.Universal[Int]

      /*
      object Undirected {
        object OnInts extends Undirected[Int] 
      }
      */


      trait Directed[V] extends EdgeSet[V] 

      /*
      object Directed {
        object OnInts extends Directed[Int] 
      }
      */

    }
  }
}




