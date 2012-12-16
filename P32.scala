object P32 extends Euler {

  import Common.factors

  import Utils._

  val dMemo = new Memo[Int, Seq[Char]]
  val iMemo = new Memo[Int, Boolean]

  def digits(n: Int) = dMemo(n) { n.toString.toSeq }
  def isPanish(n: Int) = iMemo(n) {
    val d = digits(n)
    val ds = d.toSet
    (ds.size == d.size) && !(ds contains '0')
  }

  val cands = for { 
    n <- (1234 to 9876).view 
    if isPanish(n)
  } yield n
    

  def pandigitalFacts(n: Int) = {
    val ds = digits(n) toSet
    val f = factors(n) find { f =>
      val d = n / f 
      (d * f == n) && 
      isPanish(f) &&
      isPanish(d) &&
      (ds.size + digits(d).size + digits(f).size == 9) &&
      (ds ++ digits(d) ++ digits(f)).size == 9
    }
    f map (f => (f, n/f)) 
  }

  lazy val pandigitalProds = for {
    cand <- cands
    (a,b) <- pandigitalFacts(cand)
  } yield (BigInt(cand), a, b)

  def soln =
    pandigitalProds map (_._1) sum
      

      
  
  
}


// vim: set ts=2 sw=2 et:
