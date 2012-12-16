object P21 extends Euler {

  import Common._

  lazy val memo = new Memo[Int, Option[Int]]

  
  def d(n:Int) = memo(n) {
    factors(n) filterNot(n ==) reduceOption (_+_)
  }

  def amicableTill(n:Int) = for { 
    a <- 2 until n
    b <- d(a) if a != b
    aa <- d(b) if a == aa
  } yield a

  def soln = amicableTill(10000) reduce (_+_)
}

// vim: set ts=2 sw=2 et:
