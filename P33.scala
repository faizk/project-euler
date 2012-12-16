object P33 extends Euler {

  import Common.PF

  import P32.{digits,isPanish}

  class Frac(val n:Int, val d:Int) {
    def *(that: Frac): Frac = Frac(n*that.n, d*that.d)
    def /(that: Frac): Frac = Frac(n*that.d, d*that.n)
    override
    def toString = "%d/%d".format(n,d)
  }
  object Frac {
    def apply(n:Int, d:Int) = 
      (new Frac(_,_)) tupled reduce(n, d)
    def unapply(f: Frac) = Option(f).map(f => (f.n, f.d))
    def reduce(n:Int, d:Int) = {
      val npfs = PF(n) toMap
      val dpfs = PF(d) toMap
      val (PF(rn), negs) = 
        (npfs.keySet ++ dpfs.keySet).toSeq map { p =>
        val npow = npfs.getOrElse(p, 0) 
        val dpow = dpfs.getOrElse(p, 0)
        (p, npow-dpow)
      } partition { case (p, pow) => pow > 0 } 

      val PF(rd) = negs collect { case (p, pow) if pow < 0 => (p, -pow) }

      (rn, rd)
    }
  }


  val cands = for {
    n <- 11 to 99 if isPanish(n)
    d <- 11 to 99 if isPanish(d)
    if d > n
    val (ds, ns) = (digits(d), digits(n))
    c <- ns intersect ds headOption;
    d1 <- Some(ds.filterNot(_==c).toString toInt)
    n1 <- Some(ns.filterNot(_==c).toString toInt)
    if (n:Double)/d == (n1:Double)/d1
  } yield Frac(n, d)




  def soln = 
    cands reduce ((a,b) => a*b)

}

// vim: set ts=2 sw=2 et:
