object P23 extends Euler {

  def sumOfPropDiv(n: Int) = P21.d(n)

  val memo = new Memo[Int, Boolean] 

  def isAbundant(n: Int) = memo(n) {
    sumOfPropDiv(n) map (_ > n) getOrElse false
  } 

  def sums(n:Int) = (1 to (n/2).toInt) map (x => (x, n-x))

  def p(n:Int) = sums(n) forall {
    case(x,y) => !(isAbundant(x) && isAbundant(y))
  }

  def soln = (1 until 28123) filter p reduce (_+_)

  // bitvector approach?
  def bitvector_approach = { 

    import Utils._

    val bv = new collection.mutable.ArraySeq[Boolean](28123 * 2)

    val abundants = (1 to 28123) filter isAbundant toIndexedSeq

    for {
      i <- abundants
      j <- abundants.slice(0,i) 
    } bv(i+j) = true

    (1 to 30000) filterNot bv reduce (_+_)
  }
}

// vim: set ts=2 sw=2 et:
