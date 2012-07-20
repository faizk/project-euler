object Utils {
  def time[A](f: => A) = { 
    import java.util.Date
    val start = new Date
    val r = f
    printf("Took: %d ms\n", (new Date getTime) - start.getTime)
    r
  }

  lazy val allNums = {
    def N(n:Int): Stream[Int] = Stream.cons(n, N(n+1))
    N(1)
  }

}

trait Euler {

  class Memo[K,V] {
    val cache = collection.mutable.Map[K, V]()
    def apply(k: K)(f: => V):V =
      cache.get(k) match {
        case Some(v) => v
        case None => {
          val v = f
          cache(k) = v
          v
        }
      }
  }


  def todo = sys.error("TODO!")
  def soln: Any
}


// vim: set ts=2 sw=2 et:
