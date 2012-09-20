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

  class Memo[K,V](off: Boolean = false) {
    val cache = collection.mutable.Map[K, V]()
    var (hits, misses) = (0,0)
    def hit  = hits = hits + 1
    def miss = misses = misses + 1 
    def apply(k: K)(f: => V):V = if (off) f else
      cache.get(k) match {
        case Some(v) => hit; v
        case None => {
          miss
          val v = f
          cache(k) = v
          v
        }
      }
    override
    def toString = if (off) "(* OFF *)" 
      else "(* hits:%d, misses:%d *)" format (hits, misses)
  }


  def todo = sys.error("TODO!")
  def soln: Any
}


// vim: set ts=2 sw=2 et:
