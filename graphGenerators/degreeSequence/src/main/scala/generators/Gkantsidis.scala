
object Gkantsidis {
  import GraphOps._

  type IntAL = Map[Int, IndexedSeq[Int]]

  def apply(degSeq: Seq[Int]): Option[Map[Int, IndexedSeq[Int]]] = {
    for{ links <- realized( randomIdx )(degSeq)
         al <- connected( adjListForLinks(links))
    } yield mcmc(al)(linksInAdjList(al).length)


  def randomIdx[T](vs: IndexedSeq[T]): Option[Int] = vs.length match {
    case 0 => None
    case n => Some(nextInt(n))
  }


  
  def realized(idx: IndexedSeq[(Int, Int)] => Option[Int])(degSeq: Seq[Int]): Option[ IndexedSeq[(Int, Int)]] = {
    def lnkd(vs: IndexedSeq[(Int, Int)], k: Int): IndexedSeq[V] = 
      vs.take(k).map{ case (l,  d) => (l, d-1)}.filter{ case (_, d1) => d > 0} ++ vs.drop(k)

    def rlzdFrom(vs: IndexedSeq[(Int, Int)], links: IndexedSeq[ (Int, Int)]): Option[IndexedSeq[(Int,Int)]] = {
      idx(vs) match{
        case None => Some(links)
        case Some(i) => {
          val v = vs(i)
          val rest = vs.take(i) ++ vs.drop(i+1)
          if (rest.length < v.degree) None
          else rlzdFrom( lnkd(rest, v._2).sortBy(_._2), links ++ rest.take(v._2).map{ case(l,_) => (l, v._1)})
        }
      }
    }
    rlzdFrom( Range(0, degSeq.length).map{ case i => (i, degSeq(i))}.sortBy(_._2), IndexedSeq[ (Int, Int)]())
  }

  def edgesToSwap(


