object Common {
 
  import P12.factors_best 
  import P10.sieve

  import language.implicitConversions

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
 

  object Conv {
    
    class Rotateable[T](s: Seq[T]) {
      private
      def rot(s: Seq[T], n:Int): Seq[T] = (n, s) match { 
        case (_, Seq()) => s
        case (0, s) => s
        case (n, s) if n > 0 => rot(s.tail++Seq(s.head), n-1)
        case (n, s) if n < 0 => rot(Seq(s.last) ++ s.slice(0, s.size-1), n+1)
      }
      def rotate(n: Int): Seq[T] = rot(s, n)
    }
    implicit def toRotateable[T](s: Seq[T]) = new Rotateable(s)

    class HasDigits(n: Int) {
      def digits = n.toString.map(_-48)
    }
    implicit def toHasDigits(n:Int) = new HasDigits(n)

  }
}                          
// vim: set ts=2 sw=2 et:  
                           
                           
