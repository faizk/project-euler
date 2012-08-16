object P12 extends Euler {

  import scala.collection.mutable.ListBuffer

  import Utils._

  def tn(n: Int) = (1 to n) reduce (_+_) 

  def factors_brute(n: Int) = 
    (1 to (n/2).toInt) filter (x => n % x == 0)
 
  def factors_best(n: Int) = 
    (1 to math.sqrt(n).toInt) filter (n % _ == 0) flatMap (x => Set(x, n/x)) 

  type Primes = Array[Int]

  lazy val p500 = 
    allNums map (x => (x, P10 sieve x)) find (_._2.size > 500) match {
      case Some((n, primes)) => primes
      case _ => sys.error("can't get 500 primes!")
    }
 
  def divMod(x: Int, d: Int) = (x/d, x%d)

  def scan(n: Int, ps: Primes = p500) = {
    var d = n
    def divisibleBy(p: Int) = {
      var rem = 0
      val l = new ListBuffer[Int]
      while (rem == 0) {
        divMod(d, p) match {
          case (d1, 0) => { d = d1; rem = 0; l += p; }
          case (_, rem1) => { rem = rem1 }
        }
      }
      l
    }
    for {
      p <- ps
      f <- divisibleBy(p)
    } yield f
  }

  def factors(n: Int) = {
    def prod(s: List[Int]) = s reduce (_*_)
    val ps = p500
    val fs = for {
      s <- scan(n, ps).zipWithIndex.toSet.subsets if !s.isEmpty
    } yield (prod(s.toList map (_._1))) 
    1 :: fs.toList.distinct
  }


  def soln = allNums.map(tn) .
    map (n=> (n, factors(n) size)) .
    find (_._2 > 500)
}


// vim: set ts=2 sw=2 et:
