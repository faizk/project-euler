object P34 extends Euler {

  def fact(n:Int):Int = if (n <= 1) 1 else  n * fact(n-1)

  val f9 = fact(9)

  val upperBound = 1 to 100 find 
    (d => math.pow(10,d) > f9*d ) map 
    (f9*_ + 1) get

  import Common.Conv.toHasDigits

  def soln = (3 to upperBound) filter { i =>
    i.digits.map(fact).sum == i 
  } sum

}


// vim: set ts=2 sw=2 et:
