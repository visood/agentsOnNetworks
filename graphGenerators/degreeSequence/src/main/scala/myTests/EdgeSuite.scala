package learn.graph
import scala.language.higherKinds

import GraphEdge._
import MyTest._




object EdgeTest{
  import WorkAndLeisure._

  def run{
    MyTest.Equality(WorkCnxn(1, 2), WorkCnxn(2,1) )
    MyTest.Inequality(WorkCnxn(1,2), WorkCnxn(1, 3))
    MyTest.Equality(BarCnxn(1,2), BarCnxn(2,1))
    MyTest.Inequality(WorkCnxn(1,2), BarCnxn(1,2))
    MyTest.Inequality(WorkCnxn(1,2), BossEmployee(1,2))
    MyTest.Inequality(BossEmployee(1,2), WorkCnxn(1,2))
    MyTest.Inequality(BossEmployee(1,2), BossEmployee(2,1))
  }
}

