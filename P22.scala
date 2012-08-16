object P22 extends Euler {
  val names = io.Source.fromFile("names.txt").
    mkString split (",") map (_.replace("\"","")) sorted

  def soln = 
    (names map (n => n map (c => c.toLong - 64) reduce (_+_))).zipWithIndex.
    map { case (v,i) => v * (i+1) } reduce (_+_)

}

// vim: set ts=2 sw=2 et:
