package learn.graph
import scala.language.higherKinds

import GraphEdge._
import MyTest._
import Endogeneous._
import Endogeneous.Link._

object WorkAndLeisure {
  trait Work extends Context { val context = "Work" }
  case object Work extends Work

  case class WorkCnxn[V](val _1: V, val _2: V) extends Link.Bond[V] with Work {
    def asEdge(x: V, y: V) = WorkCnxn(x, y)
  }

  trait Bar extends Context{ val context = "Bar"}
  case object Bar extends Bar
  case class BarCnxn[V](val _1: V, val _2: V) extends Link.Bond[V] with Bar {
    def asEdge(x: V, y: V) = BarCnxn(x, y)
  }

  case class BossEmployee[V](val _1: V, val _2: V) extends Link.Arrow[V] with Work {
    def asEdge(x: V, y: V) = BossEmployee(x, y)
  }

}

