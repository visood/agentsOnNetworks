package learn.graph
import scala.util.Random._
import breeze.stats.distributions._


	/** 
		* An implementation of The Markov Chain Simulation Method for Generating Connected
		* Power Law Random Graph
		*/
object Gkantsidis extends App{


  import Graph._

  def apply(degSeq: Seq[Int]): Option[AL] = {
    realized( (vs: IndexedSeq[Vertex]) => randomIdx(vs)) (degSeq) match{
      case None => None
      case Some(links) => connected(adjListForLinks(links))
      //case Some(links) => mcmcd( connected( adjListForLinks(links) ) )
    }
  } 
	


  case class Vertex(val label: Int, val degree: Int)
  
  implicit object DegOrdering extends Ordering[Vertex] {
    def compare(v: Vertex, u: Vertex) = -(v.degree compare u.degree)
  }

  def randomIdx(vs: IndexedSeq[Vertex]): Option[Int] = vs.length match{
    case 0 => None
    case n => Some(nextInt(n))
  }

  def realized(idx: IndexedSeq[Vertex] => Option[Int])(degSeq: Seq[Int]): Option[ IndexedSeq[(Int, Int)]] = {
    //this algorithm is not correct, or I miss-interpreted
    def linked(vs: IndexedSeq[Vertex], k: Int): IndexedSeq[Vertex] = 
      vs.take(k).map{ case Vertex(l, d) => Vertex(l, d - 1)}.filter{case Vertex(_, d1) => d1 > 0} ++ vs.drop(k)

    def rlzdFrom( vs: IndexedSeq[Vertex], links: IndexedSeq[(Int, Int)]): Option[IndexedSeq[(Int, Int)]] = {
     idx(vs) match{
      case None => Some(links)
      case Some(i) => {
        val v = vs(i)
        val rest = vs.take(i) ++ vs.drop(i+1)
        if (rest.length < v.degree) None 
        else rlzdFrom( linked(rest, v.degree).sorted, links ++ rest.take(v.degree).map{ case Vertex(l, _) => (l, v.label)})
      }
    }
   }
   rlzdFrom( (Range(0, degSeq.length).map{ case i => Vertex(i, degSeq(i))}).sorted, IndexedSeq[(Int, Int)]())
  }


  //def connectTwo(oal1: Option[AL], oal2: Option[AL]): Option[AL] = (oal1, oal2) match {
  def connected(oal1: Option[AL], oal2: Option[AL]): Option[AL] = (oal1, oal2) match {
    case (None, None) => None
    case (None, Some(al2)) => Some(al2)
    case (Some(al1), None) => Some(al1)
    case (Some(al1), Some(al2)) => {  val e12 = randomLinkOnCycle(al1) match {
                                                  case None =>  randomLinkOnCycle(al2) match { 
                                                                  case None => None
                                                                  case Some(e2) =>  randomLink(al1) match {
                                                                                      case None => None
                                                                                      case Some(e1) => Some( (e1, e2))
                                                                                    }
                                                                }
                                                  case Some(e1) =>  randomLink(al2) match {
                                                                     case None => None
                                                                     case Some(e2) => Some( (e1, e2) )
                                                                    }

                                      } 
                                      e12 match {
                                            case None => None
                                            case Some((e1, e2)) =>  { val (f1, f2) = edgeSwap(e1, e2)
                                                                      Some( linksAdded(linksRmvd(al1)(e1) ++ linksRmvd(al2)(e2))(f1, f2) )
                                                                    }
                                        }
                                   }
    }

  def connected( al: AL): Option[AL] = {
    val cmps = components(al).sortBy{ case c => - numLinksOnCycle( projection(al)(c) )}
    ( (None: Option[AL]) /: cmps){ case (ola, c) => connected(ola, projection(al)(c)) }
    //( Some( projection(al)(cmps.head) ) /: cmps.tail){ case (ola, c) => connected(ola, projection(al)(c)) }
  }

}

    


