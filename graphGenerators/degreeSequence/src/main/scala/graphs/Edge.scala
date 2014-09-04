package learn.graph
import scala.language.higherKinds
/** 2014 08 28
  * Lets not worry about hyper-graphs or hyper-edges
  * Code something clean and expressive
  * An edge represents a relationship between two vertexes
  */


object GraphEdge{
   
  trait Context{
    val context: String
    def contextually(other: Context): Boolean = other match {
      case that: Context.Universe => true
      case that => that.context == this.context
    }
  }
  object Context{
    trait Universe extends Context {
      val context: String = "All"
      override def contextually(that: Context): Boolean = true
    }
    case object Universe extends Universe
  }


  trait Relation[V, U]{
    this: Product2[V, U] => 

    def pair: (V, U) = (_1, _2)
    def order: Set[(V, U)]
    def sources: Set[V]
    def sinks: Set[U]
    def canEqual(other: Any): Boolean = other match {
      case that: Relation[_, _] with Product2[_, _] => this._1 == that._1 && this._2 == that._2
      case _ => false
    }

    def relationally[T, S](that: Relation[T, S] with Product2[T, S]): Boolean = this canEqual that
  }

  object Relation{
   
      trait Exogeneous[V, U] extends Relation[V, U] {
        this: Product2[V, U] =>

        def order: Set[(V, U)] = Set((_1, _2))
        def sources: Set[V] = Set(_1)
        def sinks: Set[U] = Set(_2)
      }

      trait Symm[V] extends Relation[V, V]  {
        this: Product2[V, V] => 

        def order: Set[(V, V)] = Set( (_1, _2), (_2, _1) )
        def sources: Set[V] = Set(_1, _2)
        def sinks: Set[V] = Set(_1, _2)
        override def relationally[T, S](other: Relation[T, S] with Product2[T, S]): Boolean = other match {
          case that: Symm[_] with Product2[V, V] => 
            (super.relationally(that)) || (this._1 == that._2 && this._2 == that._1)  
          case _ => false
        }
      }

      trait Asym[V] extends Relation[V, V] {
        this: Product2[V, V] => 

        def order: Set[(V, V)] = Set( (_1, _2) )
        def sources: Set[V] = Set(_1)
        def sinks: Set[V] = Set(_2)
        override def relationally[T, S](other: Relation[T, S] with Product2[T, S]): Boolean = other match {
          case that: Asym[_] with Product2[V, V] => super.relationally(that)
          case _ => false
          }
      } 
  }


  trait Edge[V, U] {
    this: Product2[V, U] with Relation[V, U] with Context => 
    
    def asEdge( x: V, y: U): Edge[V, U] //necessary?
    def ends: (V, U) = (_1, _2)

    override def equals(other: Any): Boolean = other match {
      case that: Edge[V, U] with Product2[V, U] with Relation[V, U] with Context=> 
        relationally(that) && contextually(that)
      case _ => false
    }
    override def hashCode = order.map(_.hashCode).sum/2
  }

  trait TypeClass[V, U] {
    def asEdge(e: (V, U) ): Edge[V, U] with Product2[V, U] with Relation[V, U] with Context
    def asEdge(x: V, y: U): Edge[V, U] with Product2[V, U] with Relation[V, U] with Context = asEdge( (x, y) )
  }



/** 
  * Endogeneous graphs: with only one vertex type
  */

  object Endogeneous{

    trait Link[V] extends Edge[V, V] with Product2[V, V] with Relation[V, V] with Context

    object Link {
      trait TypeClass[V]  {
        this: Context => 

        def asEdge(e: (V, V)): Link[V]
        def asEdge(x: V, y: V): Link[V] = asEdge( (x, y) )
      }

      trait Bond[V] extends Link[V] with Relation.Symm[V] {
        this: Context =>
      }

      object Bond {
        case class Universal[V](val _1: V, val _2: V) extends Bond[V] with Context.Universe {
          def asEdge(x: V, y: V): Bond.Universal[V] = Universal(x, y)
        }
        object TypeClass {
          trait Universal[V]  extends Link.TypeClass[V] with Context.Universe {
            def asEdge(e: (V, V) ) = Bond.Universal(e._1, e._2)
          }
        }
      }

      trait Arrow[V] extends Link[V] with Relation.Asym[V] {
        this: Context =>
      }

      object Arrow {
        case class Universal[V](val _1: V, val _2: V) extends Arrow[V] with Context.Universe {
          def asEdge(x: V, y: V): Arrow.Universal[V] = Universal(x, y)
        }

        object TypeClass {
          trait Universal[V] extends Link.TypeClass[V] with Context.Universe {
            def asEdge(e: (V, V) ) = Arrow.Universal(e._1, e._2)
          }
        }
      }
    }
  }




  object Bipartite {
    trait BiEdge[V, U] extends Edge[V, U] with Product2[V, U] with Relation.Exogeneous[V, U] {
      this: Context => 
      def inverse: BiEdge[U, V]
    }

    object BiEdge {
      trait TypeClass[V, U] {
        def asEdge(e: (V, U)): BiEdge[V, U]
      }
    }
  }

}
