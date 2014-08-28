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
    def canEqual(other: Any): Boolean = other match {
      case that: Relation[_, _] with Product2[_, _] => this._1 == that._1 && this._2 == that._2
      case _ => false
    }

    def relationally(that: Relation[V, U] with Product2[V, U]): Boolean = this canEqual that
  }

  object Relation{

    trait Symm[V] extends Relation[V, V]  {
      this: Product2[V, V] => 

      def order: Set[(V, V)] = Set( (_1, _2), (_2, _1) )
      override def relationally(other: Relation[V, V] with Product2[V, V]): Boolean = other match {
        case that: Symm[_] with Product2[V, V] => 
          (super.relationally(that)) || (this._1 == that._2 && this._2 == that._1)  
        case _ => false
      }
    }

    trait Asym[V] extends Relation[V, V] {
      this: Product2[V, V] => 

      def order: Set[(V, V)] = Set( (_1, _2) )
    }
  }


  trait Edge[V, U] {
    this: Product2[V, U] with Relation[V, U] with Context => 

    def ends: (V, U) = (_1, _2)

    override def equals(other: Any): Boolean = other match {
      case that: Edge[V, U] with Product2[V, U] with Relation[V, U] with Context=> 
        relationally(that) && contextually(that)
      case _ => false
    }
    override def hashCode = order.map(_.hashCode).sum/2
  }
}

