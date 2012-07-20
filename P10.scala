object P10 extends Euler {

  import collection.mutable._
  
  def sans(d: Int) = (x:Int) => x % d == 0

  def sieve_strawman(n: Int) = {
    val r = new ListBuffer[Int]
    var l = (2 to n) toList
    
    def sieve(l: List[Int], d: Int) = 
      (l filterNot (sans(d)))

    while (!l.isEmpty) {
      r += l.head
      println("sieving: " + l.head)
      l = sieve(l, l.head) 
    }
    r
  }


  def sieve(n: Int) = {
    val p = (0 to n) map (n => true) toArray
    import math.pow
    Seq(0,1) foreach (p(_) = false)

    2 until p.size foreach { i =>
      if (p(i)) 
        (pow(i,2).toInt to n by i) foreach (p(_) = false)
    }

    for {
      (b, i) <- p.zipWithIndex if b
    } yield i

  }

  def soln = sieve(2*1000*1000) map (_.toLong) reduce (_+_)


}



// vim: set ts=2 sw=2 et:
