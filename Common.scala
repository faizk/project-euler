object Common {
 
  import P12.factors_best 
  import P10.sieve

  def factors(n:Int) = P12.factors_best(n)


  def primeFactors(n:Int) = {
    val fs = factors(n)
    val ps = sieve(n)
    fs intersect ps
  }

  
  def biggest(n: Int, fs: Set[Int]) = {
    def next(n: Int, p: Int): Option[Int] = 
      if (fs contains math.pow(n, p+1).toInt) next(n, p+1)
      else if (p == 0) None else Some(p)
    next(n, 0)
  }
     
  object PF {

    def apply(n: Int) = {
      val fs = factors(n).toSet
      for {
        _ <- List(n) if n > 0
        p <- sieve(n) if p == n || p <= n/2
        pow <- biggest(p, fs) 
      } yield (p, pow)
    } seq

    def unapply(fp: Seq[(Int, Int)]): Option[Int] = 
      fp map { case (f,p) => math.pow(f,p) } reduceOption (_*_) map (_.toInt) orElse Some(1)
                           
  }                        
                           
}                          
// vim: set ts=2 sw=2 et:  
                           
                           
