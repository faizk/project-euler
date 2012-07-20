object P14 extends Euler {

  import Utils._

  lazy val cache = collection.mutable.Map[Long, List[Long]]()


  def just_collatz(n: Long):List[Long] = {
    n :: (n match { 
      case 1 => List()
      case n if (n%2==0) => just_collatz(n/2)
      case _ => just_collatz((3*n)+1) 
    })
  }

  def collatz(n: Long): List[Long] = cache.get(n) match {
    case Some(list) => {
      list
    }
    case None => {
      val l = n :: (n match { 
          case 1 => List()
          case n if (n%2==0) => collatz(n/2)
          case _ => collatz((3*n)+1) 
        })
      cache(n) = l
      l
    }
  }

  def collatz_non_rec(n: Long) = {
    var x = n
    val l = new collection.mutable.ListBuffer[Long]
    l += x
    while (x > 1) {
      x = if (x%2 == 0) x/2 else (3*x)+1
      l += x
    }
    l
  }



  def soln = (1 until 1000*1000).view.reverse maxBy (collatz(_).size)

}

// vim: set ts=2 sw=2 et:
