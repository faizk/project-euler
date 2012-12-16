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


  def sieve1(n: Int) = {
    val s = collection.mutable.Set[Int]()
    val p = new {
      def apply(n: Int) = !s.contains(n)
      def update(n:Int, b:Boolean) = 
        if (b) s -= n else s += n
    }
    import math.pow
    Seq(0,1) foreach (p(_) = false)
    2 until n foreach { i =>
      if (p(i)) 
        (pow(i,2).toInt to n by i) foreach (p(_) = false)
    }
    s
  }

    

  def sieve(n: Int) = {
    val p = Array.fill(n+1)(true)
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
