package learn.graph
import scala.language.higherKinds

import GraphEdge._

    
object MyTest{

  def Comparison[T, R](f: (T, R) => Boolean, desc: String)(a: T, b: R): Unit = {
    try{ 
      assert(f(a,b))
      println(s"PASSED, $a $desc $b")
    } catch {
      case e: java.lang.AssertionError => println(s"FAILED $a $desc $b")
    }
  }
  def Equality[T, R] = Comparison( (a: T, b: R) => (a == b), desc = "==" ) _
  def Inequality[T, R] = Comparison( (a: T, b: R) => ( a != b), desc = "!=" ) _
}

object EdgeTest{
  trait Work extends Context { val context = "Work" }
  case object Work extends Work

  case class WorkCnxn[V](val _1: V, val _2: V) extends Edge[V, V] with Product2[V, V] with Relation.Symm[V] with Work

  trait Bar extends Context{ val context = "Bar"}
  case object Bar extends Context {val context = "Bar"}
  case class BarCnxn[V](val _1: V, val _2: V) extends Edge[V, V] with Product2[V, V] with Relation.Symm[V] with Bar

  case class BossEmployee[V](val _1: V, val _2: V) extends Edge[V, V] with Product2[V, V] with  Relation.Asym[V] with Work

  case class RandomFriends[V](val _1: V, val _2: V) extends Edge[V, V] with Product2[V, V] with Relation.Symm[V] with Context.Universe

  def run{
    MyTest.Equality(WorkCnxn(1, 2), WorkCnxn(2,1) )
    MyTest.Inequality(WorkCnxn(1,2), WorkCnxn(1, 3))
    MyTest.Equality(BarCnxn(1,2), BarCnxn(2,1))
    MyTest.Inequality(WorkCnxn(1,2), BarCnxn(1,2))
    MyTest.Inequality(WorkCnxn(1,2), BossEmployee(1,2))
    MyTest.Inequality(BossEmployee(1,2), BossEmployee(2,1))
  }
}

