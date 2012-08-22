object P28 extends Euler {

  lazy val jumps = 3 to 1001 by 2 map (s => Vector(s-1,s-1,s-1,s-1)) flatten

  def soln = (List(1) /: jumps) { (l,b) =>  (l.head + b) :: l } reduce (_+_)

}

// vim: set ts=2 sw=2 et:
