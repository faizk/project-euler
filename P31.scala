
object P31 extends Euler {

  val deNoms = Set(1, 2, 5, 10, 20, 50, 100, 200)

  def m(t: Int, d: Int): Iterable[Int] = 0 to (t/d) map (_*d)

  object Pick {
    def unapply(s: Set[Int]): Option[(Int, Set[Int])] = 
      s reduceOption math.max map (m => (m, s-m))
  }

  val memo = new Memo[(Int, Set[Int]), Int]

  def f(t: Int, ds: Set[Int]): Int = memo((t, ds)) {
    t match {
      case 0 => 1
      case _ => 
        ds match {
          case Pick(max, rest) if rest.isEmpty => 
            if (t %  max == 0) 1 else 0
          case Pick(max, rest) =>
            m(t, max) map (d=>f(t-d, rest)) sum
          case _ => 0
        }
    }
  }

  def soln = f(200, deNoms)


    /**


  def changeR(money: Int, coins: List[Int]):Int = {
    if (money == 0) 1
    else if (coins.isEmpty) 0
    else {
      val (d, rest) = (coins.head, coins.tail)
        (0 to money by d) map (d => changeR(money - d, rest)) sum 
    }
  }

  def countChange(money: Int, coins: List[Int]): Int = 
    if (money == 0) 1
  else if (money < 0) 0
  else if (coins.isEmpty) 0
  else 
    countChange(money, coins.tail) +
  countChange(money - coins.head, coins) 



  */


}

// vim: set ts=2 sw=2 et:
