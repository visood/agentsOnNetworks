import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

//libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

import GraphsOps._
import GraphTypeClass._
@RunWith(classOf(JUnitRunner))

/**
  * For now we write tests to be run manually,
  * Incorporate these into ScalaTest based tests
  * when we learn ScalaTest
  */

object GraphTypeClassTests{
    /**
      * To use an adjacency matrix or an edge set as graph representation,
      * we have to declare their type as AL[V, E] and ESet[V, E]
      * this allows the compiler to plug in the appropriate implicits
      */


  /**
    * Some tests
    * Try to implement using ScalaTest in the directory test
    */
  implicit def glalu = GraphLike.AdjList.Undirected[Int]
  implicit def glesu = GraphLike.EdgeSet.Undirected[Int]
  val al = Map( 1 -> IndexedSeq(2,3), 2 -> IndexedSeq(1), 3 -> IndexedSeq(1))
  val gal: AL[Int, Bond] = graphLike[Int, Bond, AL](al)
  val es = Set( (2,3), (1,2) )
  val ges: ESet[Int, Bond] = graphLike[Int, Bond, ESet](es)
  try{
    assert(vertexes(gal).toSet == Set(1,2,3))
    println(s"Vertexes of $gal passed")
  } catch {
    case e: Exception => println(s"Vertexes of $gal failed")
  }
  try{
    assert(vertexes(ges).toSet == Set(1,2,3))
    println(s"Vertexes of $ges passed")
  } catch {
    case e: Exception => println(s"Vertexes of $ges failed")
  }
  try{
    assert(vertexSet(ges) == Set(1,2,3))
    println(s"Vertex Set of $ges passed")
  } catch {
    case e: Exception => println(s"Vertex Set of $ges failed")
  }
  try{
    assert(vertexSet(gal) == Set(1,2,3))
    println(s"Vertex Set of $gal passed")
  } catch {
    case e: Exception => println(s"Vertexes of $gal failed")
  }
  try{
    assert(degrees(gal) == Map(1 -> 2, 2 -> 1, 3 -> 1) )
    println(s"Degrees of $gal passed")
  } catch {
    case e: Exception => println(s"Degrees of $gal failed")
  }
  try{
    assert(degrees(ges) == Map(1 -> 1, 2 -> 2, 3 -> 1) )
    println(s"Degrees of $ges passed")
  } catch {
    case e: Exception => println(s"Degrees of $ges failed")
  }
  try{
    assert(adjList(gal) == al)
    println(s"Adjacency List of $gal passed")
  } catch {
    case e: Exception => prinln(s"Adjacency List of $gal failed")
  }
  try{
    assert(adjList(ges) == Map(1 -> IndexedSeq(1), 2 -> IndexedSeq(1, 3), 3 -> IndexedSeq(2)) )
    println(s"Adjacency List of $ges passed")
  } catch {
    case e: Exception => prinln(s"Adjacency List of $ges failed")
  }
  try{
    assert(edgeEnds(ges) == Map(1 -> IndexedSeq(1), 2 -> IndexedSeq(1, 3), 3 -> IndexedSeq(2)) )
    println(s"Adjacency List of $ges passed")
  } catch {
    case e: Exception => prinln(s"Adjacency List of $ges failed")
  }
  def edgeSet1 = edgeSet(al)
  def edgeSet2 = edgeSet(es)
  def tryToMerge1 = merger(al, es)
  def tryToMerge2 = merger(es, al)
  def tryToDiffer1 = differ(al, es)
  def tryToDiffer2 = differ(es, al)
}
