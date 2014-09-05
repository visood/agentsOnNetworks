package learn.graph
import scala.util.Random._

object Gkantsidis {
  import GraphTypeClass._
  import GraphFunctions._
  import GraphEdge.Endogeneous._

  type AL = GraphLike.AL[Int]

  type IQ[T] = IndexedSeq[T]

  object glal extends  GraphLike.AdjList.Undirected[Int] with Transformations[Int] with Structures[Int] with Properties[Int]

  implicit object evel extends Link.Bond.TypeClass.Universal[Int]

  def apply(degSeq: Seq[Int]): Option[Map[Int, IQ[Int]]] = {
    for{ 
      links <- realized(degSeq)
      al <- glal.swapConnected( glal.from(links.toSet))
    } yield glal.mcmc(al)(glal.numEdges(al))
  }

  case class Vertex(val label: Int, val degree: Int)

  implicit object DegOrdering extends Ordering[Vertex] {
    def compare(v: Vertex, u: Vertex) = (v, u) match { case (Vertex(_, n), Vertex(_, m) ) => - (n compare m) }
  }

  case class Split( val v: Vertex, val toLink: IQ[Vertex], val rest: IQ[Vertex])
  def randomSplit(vs: IQ[Vertex]): Option[Split] = vs match {
    case IndexedSeq() => None
    case _ => { 
      val i = nextInt(vs.length)
      (vs(i), vs.take(i) ++ vs.drop(i + 1)) match {
        case (Vertex(l, k), rest) => Some( Split(Vertex(l, k), rest.take(k), rest.drop(k)) )
      }
    }
  }

  def realized(degSeq: Seq[Int]): Option[ IQ[ (Int, Int) ] ] = {
    def linksBw( vs: IQ[Vertex], l: Int): List[ (Int, Int)] = vs.map{ case Vertex(m, _) => (m, l) }.toList

    def linked( vs: IQ[Vertex]): IQ[Vertex] = vs.collect{ case Vertex(m, k) if (k > 1) => Vertex(m, k - 1)}

    def from( vs: IQ[ Vertex], links: IQ[ (Int, Int) ] ): Option[ IQ[ (Int, Int) ]] = randomSplit(vs) match {
      case None => Some(links)
      case Some(Split(Vertex(_, k), toLink, _)) if (k > toLink.length) => None
      case Some(Split(Vertex(l, k), toLink, rest)) => from( linked(toLink) ++ rest, links ++ linksBw(toLink, l))
    }
    from( Range(0, degSeq.length).map{ case i => Vertex(i, degSeq(i))}.sorted, IndexedSeq[(Int, Int)]())
  }


/*
  trait DegSeqPreservingAL extends GraphLike.AdjList[Int] {

    def connected(al: AL): Option[AL] = 

  }
  */

  

}
