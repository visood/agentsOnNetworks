package learn.graph
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
