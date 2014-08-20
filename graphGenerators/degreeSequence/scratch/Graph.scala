package learn.graph
import scala.util.Random._

object Graph{
  type AL = Map[Int, IndexedSeq[Int]]

	def vertexes(al: AL): IndexedSeq[Int] = (al.keys ++ al.values.flatten).toIndexedSeq
