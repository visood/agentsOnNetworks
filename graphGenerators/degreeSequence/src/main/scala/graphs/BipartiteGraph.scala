package learn.graph
import scala.util.Random._
import scala.language.higherKinds
import scala.language.postfixOps
import scala.Predef

object BipartiteGraph {
  import GraphEdge.Bipartite._
  import GraphTypeClass._
  import GraphEdge.Endogeneous._


  trait BipartiteLike[V, W <: V, U <: V, G[_, _]] {
      
    def vertexes(g: G[W, U]): Set[V] = sources(g) ++ sinks(g)
    def sources(g: G[W, U]): Set[W]
    def sinks(g: G[W, U]): Set[U]

    def degrees(g: G[W, U])(implicit eve: BiEdge.TypeClass[W, U]): Map[W, Int] = {
      val al: Map[W, IndexedSeq[U]] = adjList(g)
      al.map{ case (x, ys) => (x, ys.length) }
    }


    def edges(g: G[W, U])(implicit eve: BiEdge.TypeClass[W, U]): Set[BiEdge[W, U]]

    def adjList(g: G[W, U]): Map[W, IndexedSeq[U]]

    def from(es: Set[BiEdge[W, U]]): G[W, U]

    def from(ts: Set[(W, U)])(implicit eve: BiEdge.TypeClass[W, U], evd: DummyImplicit): G[W, U] = {
      from( ts.map{ case (x, y) => eve.asEdge(x, y) })
    }

    def from(al: Map[W, IndexedSeq[U]])(implicit eve: BiEdge.TypeClass[W, U]): G[W, U] = {
      val ts: Set[(W, U)] = al.toSeq.flatMap{ case (x, ys) => ys.map( (x, _) ) }.toSet
      from(ts)
    }

    def merger[H[_, _]](g: G[W, U], h: H[W, U])
      (implicit evh: BipartiteLike[V, W, U, H], eve: BiEdge.TypeClass[W, U]): G[W, U] = {
      from(edges(g) ++ evh.edges(h) )
    }

    def differ[H[_, _]](g: G[W, U], h: H[W, U])
      (implicit evh: BipartiteLike[V, W, U, H], eve: BiEdge.TypeClass[W, U]): G[W, U] = {
      from(edges(g) -- evh.edges(h))
    }

    def inverse(g: G[W, U])
      (implicit evi: BipartiteLike[V, U, W, G], eve: BiEdge.TypeClass[W, U], evei: BiEdge.TypeClass[U, W]): G[U, W] = {
      evi.from( edges(g).map(_.inverse) )
    }

    def project(g: G[W, U])
      (implicit ev: GraphLike[U, G[U, U]], eve: BiEdge.TypeClass[W, U], evl: Link.TypeClass[U]) : G[U, U] = {
      val es: Set[ (U, U) ] = (for{ xs <- adjList(g).values; x <- xs; y <- xs if y != x  } yield (x, y)).toSet
      ev.from(es)
    }


  }



  object BipartiteLike {

    type AL[W, U] = Map[W, IndexedSeq[U]]
    trait AdjList[V, W <: V, U <: V] extends BipartiteLike[V, W, U, AL ]{

      def sources(g: AL[W, U]): Set[W] = g.keys.toSet
      def sinks(g: AL[W, U]): Set[U] = g.values.flatten.toSet

      def edges(g: AL[W, U])(implicit eve: BiEdge.TypeClass[W, U]): Set[BiEdge[W, U]] = {
        g.toSeq.flatMap{ case (x, ys) => ys.map{ case y => eve.asEdge(x, y) } }.toSet
      }

      def from(es: Set[BiEdge[W, U]]): AL[W, U] = {
        es.toSeq.map( _.ends ).groupBy( _._1).map{ case (x, ys) => (x, ys.map(_._2).toIndexedSeq) } 
      }

      override def from(al: AL[W, U])(implicit eve: BiEdge.TypeClass[W, U]): AL[W, U] = al

      def adjList(al: AL[W, U]) = al

    }

  }

}






