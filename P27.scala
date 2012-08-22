object P27 extends Euler {

  lazy val p500 = P12.p500 toSet

  def isPrime(x:Int) = p500 contains x

  def mkF(a: Int, b: Int) = 
    (x: Int) => (x*x) + (a*x) + b 

  def consecPrimes(f: Int => Int) =
    (0 to Int.MaxValue-1).view takeWhile (x => isPrime(f(x))) size

  val bs = for (p <- p500.view if p < 1000; p1 <- Seq(p, -p)) yield p1

  val as = for (a <- 3 to 999 by 2; a <- Seq(a,-a)) yield a 

  def soln = {
    // brute force...
    val all = for { 
      b <- bs.view
      a <- as.view
    } yield (consecPrimes(mkF(a,b)), (a,b))
    val (bestN, (bestA, bestB)) = all.maxBy(_._1)
    bestA * bestB
  }


}


// vim: set ts=2 sw=2 et:
