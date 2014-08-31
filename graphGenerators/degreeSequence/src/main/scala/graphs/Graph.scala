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
  import Edge._
  

  trait EdgeLike[V, E <: Edge[V, V]]{
    def asEdge(e: (V, V) ): E
    def asEdge(x: V, y: V): E = asEdge( (x, y) )
  }


  trait GraphLike[V, E <: Edge[V, V] with Context with Relation[V, V], G ]{

    this: EdgeLike[V, E] =>

    def vertexes(g: G): Set[V] 

    def degrees(g: G): Map[V, Int] = {
      val al = adjList(g)
      al.map{ case (x, ys) => (x, ys.length) }
    }

    def edges(g: G): Set[E]

    def adjList(g: G): Map[V, IndexedSeq[V]]

    def from(es: Set[E]): G

    def from(ts: Set[(V, V)])(implicit dummy: DummyImplicit): G 
      = from( ts.map{case t => asEdge(t._1, t._2)} )

    def from(al: Map[V, IndexedSeq[V]]): G = {
      val ts: Set[(V, V)] = al.toSeq.flatMap{ case (x, ys) => ys.map( (x, _) ) }.toSet
      from(ts)
    }

    def merger[H](g: G, h: H)
      (implicit ev: GraphLike[V, E, H]): G = { 
      from((edges(g) ++ ev.edges(h)))
    }

    def differ[H](g: G, h: H)
      (implicit ev: GraphLike[V, E, H]): G = {
      from((edges(g) -- ev.edges(h)))
    }
  }

  
  object GraphLike{

    /**
      * think about what imports to make
      * be guided by : "Do not mention Bond or Arrow "
      * rewrite Edges if necessary
      */
    import Edges.Edge
    
      
    type ContextualEdge[V] = Edge[V, V] with Context with Relation[V, V]
    type IL[V, E <: Edge[V, V]] = Map[V, IndexedSeq[E] ]
    trait IncidenceList[V, E <: ContextualEdge[V]] extends GraphLike[V, E, IL[V, E]] {
      this: EdgeLike[V, E] =>

      def vertexes(g: IL[V, E]): Set[V] = g.keys.toSet //assuming a vertex w/o edges is included with an empty Vector

      def adjList(g: IL[V, E]): Map[V, IndexedSeq[V]] = g.mapValues( _.map(_.ends)).mapValues(_.map(_._2))
      
      def edges(g: IL[V, E]): Set[E] = g.toSeq.flatMap(_._2).toSet

      def from(es: Set[E]): IL[V, E] = {
        es.toSeq.flatMap(_.order).groupBy(_._1).mapValues(_.map(asEdge).toIndexedSeq)
      }

    }



      
    type AL[V] = Map[V, IndexedSeq[V]]

    //trait AL[V, E <: Edge[V, V]] extends Graph[V, E]

    trait AdjList[V, E <: Edge[V, V] with Context with Relation[V, V]] extends GraphLike[V, E, AL[V]] {
      this: EdgeLike[V, E] =>

      def vertexes(g: AL[V]): Set[V] = g.keys.toSet

      def adjList(g: AL[V]): Map[V, IndexedSeq[V]] = g.asInstanceOf[Map[V, IndexedSeq[V]]]
      def edges(g: AL[V]): Set[E] = {
        g.toSeq.flatMap{ case (x, ys) => ys.map{ case y => asEdge(x, y)} }.toSet
      }

      def from(es: Set[E]): AL[V] = {
        es.toSeq.flatMap(_.order ).groupBy(_._1).map{case (x, ys) => (x, ys.map(_._2).toIndexedSeq) }
      }

    }

    object AdjList{

      case class Bond[V](val _1: V, val _2: V) extends Edge[V, V] with Product2[V, V] with Relation.Symm[V] with Context.Universe

      trait Undirected[V] extends AdjList[V, Bond[V]] {
        this: EdgeLike[V, Bond[V]] =>
      }

      object Undirected {
        object OnInts extends Undirected[Int] with EdgeLike[Int, Bond[Int]] {
          def asEdge(e: (Int, Int)): Bond[Int] = Bond(e._1, e._2)
        }
      }

     case class Arrow[V](val _1: V, val _2: V) extends Edge[V, V] with Product2[V, V] with Relation.Asym[V] with Context.Universe

     trait Directed[V] extends AdjList[V, Arrow[V]] {
       this: EdgeLike[V, Arrow[V]] =>
     }

     object Directed {
       object OnInts extends Directed[Int] with EdgeLike[Int, Arrow[Int]] {
         def asEdge(e: (Int, Int)): Arrow[Int] = Arrow(e._1, e._2)
       }
     }

    }



    type ESet[V, E <: Edge[V, V]] = Set[E]

    trait EdgeSet[V, E <: Edge[V, V] with Context with Relation[V, V]] extends GraphLike[V, E, ESet[V, E]] {
      this: EdgeLike[V, E] => 

      def vertexes(g: Set[E]): Set[V] = g.flatMap(_.ends match { case (x, y) => Set(x, y) } )

      def adjList(g: Set[E]): Map[V, IndexedSeq[V]] = 
        g.toSeq.flatMap( _.order ).groupBy(_._1).map{ case(x, ys) => (x, ys.map(_._2).toIndexedSeq) }

      def edges(g: Set[E]): Set[E] = g

      def from(es: Set[E]): Set[E] = es
    }

    object EdgeSet{

      case class Bond[V](val _1: V, val _2: V) extends Edge[V, V] with Product2[V, V] with Relation.Symm[V] with Context.Universe

      trait Undirected[V] extends EdgeSet[V, Bond[V]] {
        this: EdgeLike[V, Bond[V]] =>
      }

      object Undirected {
        object OnInts extends Undirected[Int] with EdgeLike[Int, Bond[Int]] {
          def asEdge(e: (Int, Int)): Bond[Int] = Bond(e._1, e._2)
        }
      }

      case class Arrow[V](val _1: V, val _2: V) extends Edge[V, V] with Product2[V, V] with Relation.Asym[V] with Context.Universe

      trait Directed[V] extends EdgeSet[V, Arrow[V]] {
        this: EdgeLike[V, Arrow[V]] =>
      }

      object Directed {
        object OnInts extends Directed[Int] with EdgeLike[Int, Arrow[Int]] {
          def asEdge(e: (Int, Int)): Arrow[Int] = Arrow(e._1, e._2)
        }
      }

    }

  }
}




