object P24 extends Euler {


  lazy val digits = (0 to 9) view

  def except(n:Int) = ((n+1) to 9) 

  lazy val perms = for {
    d0 <- digits
    d1 <- digits if d1 != d0
    d2 <- digits if d2 != d1 && d2 != d0
    d3 <- digits if d3 != d2 && d3 != d1 && d3 != d0
    d4 <- digits if d4 != d3 && d4 != d2 && d4 != d1 && d4 != d0
    d5 <- digits if d5 != d4 && d5 != d3 && d5 != d2 && d5 != d1 && d5 != d0
    d6 <- digits if d6 != d5 && d6 != d4 && d6 != d3 && d6 != d2 && d6 != d1 && d6 != d0
    d7 <- digits if d7 != d6 && d7 != d5 && d7 != d4 && d7 != d3 && d7 != d2 && d7 != d1 && d7 != d0
    d8 <- digits if d8 != d7 && d8 != d6 && d8 != d5 && d8 != d4 && d8 != d3 && d8 != d2 && d8 != d1 && d8 != d0
    d9 <- digits if d9 != d8 && d9 != d7 && d9 != d6 && d9 != d5 && d9 != d4 && d9 != d3 && d9 != d2 && d9 != d1 && d9 != d0
  } yield (d0, d1, d2, d3, d4, d5, d6, d7, d8, d9)


  implicit def richInt[T](n: T) = new {
    def notIn(l: T*) = l forall (_!=n)
  }

  
  lazy val fancyPerms = for {
    d0 <- digits
    d1 <- digits if d1 notIn (d0)
    d2 <- digits if d2 notIn (d0, d1)
    d3 <- digits if d3 notIn (d0, d1, d2)
    d4 <- digits if d4 notIn (d0, d1, d2, d3)
    d5 <- digits if d5 notIn (d0, d1, d2, d3, d4)
    d6 <- digits if d6 notIn (d0, d1, d2, d3, d4, d5)
    d7 <- digits if d7 notIn (d0, d1, d2, d3, d4, d5, d6)
    d8 <- digits if d8 notIn (d0, d1, d2, d3, d4, d5, d6, d7)
    d9 <- digits if d9 notIn (d0, d1, d2, d3, d4, d5, d6, d7, d8)
  } yield (d0, d1, d2, d3, d4, d5, d6, d7, d8, d9)



  def soln = (perms drop ((1000*1000)-1) head).productIterator.toList map
    (_.toString) reduce (_+_)

}

// vim: set ts=2 sw=2 et:
