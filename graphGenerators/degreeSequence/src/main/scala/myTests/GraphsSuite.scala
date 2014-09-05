package learn.graph

object GraphTypeClassTests{
  import GraphTypeClass._
  import EdgeTest._
  import GraphLike._
  import GraphEdge._
  import WorkAndLeisure._
  import Endogeneous._
  import Endogeneous.Link._
    /**
      * To use an adjacency matrix or an edge set as graph representation,
      * we have to declare their type as AL[V, E] and ESet[V, E]
      * this allows the compiler to plug in the appropriate implicits
      */


  /**
    * Some tests
    * Try to implement using ScalaTest in the directory test
    */

  object ForIncList{

    //implicit val glicu: GraphLike[Int, IL[Int]] = GraphLike.IncList.Undirected.OnInts
    implicit val glacu: GraphLike[Int, AL[Int]] = GraphLike.AdjList.Undirected.OnInts
    val workFriends: Set[Link[Int]] = Set( WorkCnxn(1,2), WorkCnxn(2,3) )
    val barFriends: Set[Link[Int]] = Set( BarCnxn(1,3))
  }

  val es: Set[Link[Int]] = Set( WorkCnxn(1,2), WorkCnxn(1, 3), WorkCnxn(2, 4), BarCnxn(1, 4), BarCnxn(1, 5))


}
