package learn.graph
import GraphTypeClass._
import Edges._

import scala.reflect.runtime.universe._
/**
  * we are getting too deeply engrossed in the scala type system
  * here are some functions to extract types of variables
  */
object TypeExtractors{
  def paramTypeSymb[T](x: T)(implicit tag: TypeTag[T]) = tag.tpe match { case TypeRef(_, symb, _) => symb}
  def paramTypeArgs[T](x: T)(implicit tag: TypeTag[T]) = tag.tpe match { case TypeRef(_, _, args) => args}
  def paramInfo[T](x: T)(implicit tag: TypeTag[T]) = tag.tpe match { case TypeRef(a, b, c) => (a, b, c) }
}


object GraphOps{
  import GraphTypeClass._
  import Edges._

  def graphLike[V, E[_] <: Edge[_], G[_, _[_]]]( ts: Set[(V, V)])
    (implicit ev: GraphLike[V, E, G]): G[V, E] = ev.from(ts)

  def graphLike[V, E[_] <: Edge[_], G[_, _[_]]]( al: Map[V, IndexedSeq[V]])
    (implicit ev: GraphLike[V, E, G]): G[V, E] = ev.from(al)
    
  def edgeSet[V, E[_] <: Edge[_], G[_, _[_]]] (g: G[V, E])
    (implicit evg: GraphLike[V, E, G]): Set[E[V]] = evg.edgeSet(g)

  def edgeList[V, E[_] <: Edge[_], G[_, _[_]]](g: G[V, E])
    (implicit evg: GraphLike[V, E, G]): List[E[V]] = evg.edgeSet(g).toList

  def edgeEnds[V, E[_] <: Edge[_], G[_, _[_]]](g: G[V, E])
    (implicit evg: GraphLike[V, E, G]): List[(V, V)] = evg.edgeEnds(g)

  def vertexSet[V, E[_] <: Edge[_], G[_, _[_]]](g: G[V, E])
    (implicit evg: GraphLike[V, E, G]): Set[V] = evg.vertexSet(g)
  def vertexes[V, E[_] <: Edge[_], G[_, _[_]]](g: G[V, E])
    (implicit evg: GraphLike[V, E, G]): IndexedSeq[V] = evg.vertexes(g)

  def adjList[V, E[_] <: Edge[_], G[_, _[_]]](g: G[V, E])
    (implicit evg: GraphLike[V, E, G]): Map[V, IndexedSeq[V]] = evg.adjList(g)

  def degrees[V, E[_] <: Edge[_], G[_, _[_]]](g: G[V, E])
    (implicit evg: GraphLike[V, E, G]): Map[V, Int] = evg.degrees(g)

  def merger[V, E[_] <: Edge[_], G[_, _[_]], H[_, _[_]]](g: G[V, E], h: H[V, E])
      (implicit evg: GraphLike[V, E, G], evh: GraphLike[V, E, H]): G[V, E] = evg.merger( g, h)

  def differ[V, E[_] <: Edge[_], G[_, _[_]], H[_, _[_]]](g: G[V, E], h: H[V, E])
      (implicit evg: GraphLike[V, E, G], evh: GraphLike[V, E, H]): G[V, E] = evg.differ( g, h)
}

//newer verstion

object GraphOps {

  def vertexes[V, E <: Edge[V] with Product2[V, V] with Relation.Symm[V] with Context, G](g: G)
    (implicit evg: GraphLike[V, E, G]): Set[V] = evg.vertexes(g)
