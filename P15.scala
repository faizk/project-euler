object P15 extends Euler {

  lazy val cache = collection.mutable.Map[(Int, Int), Long]()

  def routes(m: Int, n: Int): Long = cache.get((m,n)) match {
    case Some(x) => x
    case None => {
      val x:Long = (m,n) match {
        case (0, n) => 1
        case (1, n) => n + 1
        case (m, 0) => 1 
        case (m, 1) => m + 1
        case (m, n) =>
          (0 to n) map (routes(m-1, _)) reduce (_+_)
      }
      println("mn: " + (m,n) + " is " + x)
      cache((m,n)) = x
      x
    }
  }

  def soln = routes(20, 20)
}

// vim: set ts=2 sw=2 et:
