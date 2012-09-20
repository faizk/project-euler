object P30 extends Euler {

  def brute(p:Int) = {
    import math.pow
    def f(n:Int) = n.toString.map(_.toInt - 48) map (pow(_, p)) sum
    val n = p + 1 // We did a proof showing that p+1 is a good upper bound
    2 to pow(10,n).toInt filter (i => f(i) == i) sum 
  }
          
  def soln = brute(5)
}

// vim: set ts=2 sw=2 et:
