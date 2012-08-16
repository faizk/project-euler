object P26 extends Euler {

  import P12.{divMod,p500}

  def by(d: Int) = {
    def div(soFar: Vector[(Int,Int)], rem: Int): (Vector[Int], Vector[Int]) = {
      val (q, r) = divMod(rem*10, d)
      r match {
        case 0 => (soFar :+ (q,r) map (_._1), Vector())
        case _ if soFar contains ((q,r)) =>
          val from = soFar indexOf ((q,r))
          (soFar take from map (_._1),  soFar drop from map (_._1))
        case _ => div(soFar :+ (q,r), r)
      }
    }
    div(Vector[(Int,Int)](), 1) 
  }


  def soln = p500 takeWhile (1000 >) maxBy (d => by(d)._2.size)


}


// vim: set ts=2 sw=2 et:
