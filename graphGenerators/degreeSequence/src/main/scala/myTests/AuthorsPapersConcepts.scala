package learn.graph
/**
  * To explore graphs that contain vertexes of multiple types.
  * We consider three kinds of vertices: Papers, Authors, and Concepts
  * The data consists of Academic articles
  * Each article has some authors, and can be associated with some concepts
  * A bipartite graph can be defined between authors and papers, 
  * as well as between concepts and papers.
  * These two will induce a bipartite graph between authors and concepts,
  * as well as project co-author and co-concept networks. 
  */
  import GraphTypeClass._
  import GraphEdge._

object ResearchLiterature{

  import GraphEdge._
  import Endogeneous._
  import Endogeneous.Link._
  import BipartiteGraph._
  import GraphEdge.Bipartite._
  import GraphTypeClass._

  trait Article {
    def authors: List[String]
    def concepts: List[String]
    def title: String
    def abs: String
    def body: String
  }


  trait Vertex {
    def title: String
    def label: Int
  }


  type Data = Map[Vertex, IndexedSeq[Vertex]]

  case class Paper(val title: String, val label: Int) extends Vertex {
    def authors(al: Data): IndexedSeq[Author] = al.getOrElse(this, IndexedSeq[Vertex]()).collect {
      case a: Author => a
    }

    def concepts(al: Data): IndexedSeq[Concept] = al.getOrElse(this, IndexedSeq[Vertex]()).collect {
      case a: Concept => a
    }
    def about(C: Concept): Bond = Bond(this, C)
    def by(A: Author): Bond = Bond(this, A)
  }

  case class Author(val title: String, val label: Int) extends Vertex 

  case class Concept(val title: String, val label: Int) extends Vertex 


  type IQ[T] = IndexedSeq[T]
  

  trait LiteratureQueries {


    def writers(al: Data)(p: Paper): IndexedSeq[Author] = al.getOrElse(p, IndexedSeq[Vertex]()).collect {
      case a: Author => a
    }

    def description(al: Data)(p: Paper): IndexedSeq[Concept] = al.getOrElse(p, IndexedSeq[Vertex]()).collect {
      case c: Concept => c
    }


    def works(al: Data)(a: Author): IndexedSeq[Paper] = al.getOrElse(a, IndexedSeq[Vertex]()).collect {
      case p: Paper => p
    }

    def expertize(al: Data)(a: Author): IndexedSeq[Concept] = al.getOrElse(a, IndexedSeq[Vertex]()).collect {
      case c: Concept => c
    }


    def about(al: Data)(a: Concept): IndexedSeq[Paper] = al.getOrElse(a, IndexedSeq[Vertex]()).collect {
      case p: Paper => p
    }

    def experts(al: Data)(a: Concept): IndexedSeq[Author] = al.getOrElse(a, IndexedSeq[Vertex]()).collect {
      case a: Author => a
    }

  }


  implicit object AuthorPaperConcept extends GraphLike.AdjList[Vertex] with LiteratureQueries

  

  trait Literature extends Context { val context = "research articles"}

  val someVertexes: Set[Vertex] = Set( ("a", 1), ("b", 2), ("c", 3)).flatMap{ 
    case (x, y) => Set(Paper(x, y), Author(x, y), Concept(x, y))
  }

  case class Bond(val _1: Vertex, val _2: Vertex) extends Link.Bond[Vertex] with Literature {
    def asEdge(x: Vertex, y: Vertex) = Bond(x, y)
  }

  trait BondLike extends Link.TypeClass[Vertex] with Literature {
    def asEdge(e: (Vertex, Vertex) ) = Bond(e._1, e._2)
  }

  val es: Set[Link[Vertex]] = Set(
    Paper("a", 1) about Concept("a", 1),
    Paper("a", 1) about Concept("b", 2),
    Paper("b", 2) about Concept("a", 1),
    Paper("a", 1) by Author("a", 1),
    Paper("b", 2) by Author("b", 2)
  )

  val al: Map[Vertex, IQ[Vertex]] = Map( 
    Paper("a", 1) -> IndexedSeq( Author("a", 1), Concept("a", 1)),
    Paper("b", 2) -> IndexedSeq( Author("b", 2), Concept("b", 2), Concept("a", 1) )
  )


  implicit object eve extends Link.TypeClass[Vertex] with Literature{
    def asEdge(e: (Vertex, Vertex) ) = Bond(e._1, e._2)
  }
  implicit object glal extends GraphLike.AdjList.Undirected[Vertex]
  //implicit object gles extends GraphLike.EdgeSet.Undirected[Vertex]


  val vs = Seq(Author("a", 1), Author("b", 2), Concept("c", 1), Concept("d", 2), Paper("e", 1), Paper("f", 2))


  case class Writer(val _1: Paper, val _2: Author) extends BiEdge[Paper, Author] with Literature {
    def asEdge(x: Paper, y: Author) = Writer(x, y)
    def inverse = Work(_2, _1)
  }

  case class Work(val _1: Author, val _2: Paper) extends BiEdge[Author, Paper] with Literature {
    def asEdge(x: Author, y: Paper) = Work(x, y)
    def inverse = Writer(_2, _1)
  }

  case class Tag(val _1: Paper, val _2: Concept) extends BiEdge[Paper, Concept] with Literature {
    def asEdge(x: Paper, y: Concept) = Tag(x, y)
    def inverse = Use(_2, _1)
  }

  case class Use(val _1: Concept, val _2: Paper) extends BiEdge[Concept, Paper] with Literature {
    def asEdge(x: Concept, y: Paper) = Use(x, y)
    def inverse = Tag(_2, _1)
  }

  implicit object writerLike extends BiEdge.TypeClass[Paper, Author] {
    def asEdge(e: (Paper, Author) ) = Writer(e._1, e._2)
  }

  implicit object alAuthor extends GraphLike.AdjList.Undirected[Author]
  
  case class Coauthorship(val _1: Author, val _2: Author) extends Link.Bond[Author] with Literature {
    def asEdge(x: Author, y: Author) = Coauthorship(x, y)
  }

  implicit object authorLink extends Link.TypeClass[Author] with Literature {
    def asEdge(e: (Author, Author)) = Coauthorship(e._1, e._2)
  }


  object alWriter extends BipartiteLike.AdjList[Vertex, Paper, Author]


  def bes: Set[BiEdge[Paper, Author]] = Set(
    Writer(Paper("a", 1), Author("a", 1)),
    Writer(Paper("a", 1), Author("b", 2)),
    Writer(Paper("b", 2), Author("b", 2)),
    Writer(Paper("b", 2), Author("c", 3)),
    Writer(Paper("c", 3), Author("c", 3)),
    Writer(Paper("c", 3), Author("d", 4)), 
    Writer(Paper("d", 4), Author("d", 4))
  )


  
}


//scratch 
