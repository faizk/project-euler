object P35 extends Euler {

  import scala.language.implicitConversions
  lazy val primes = P10.sieve(1000*1000) toSet

  class RotatableInt(n:Int) {
    import Common.Conv._
    def rotations: Seq[Int] = {
      val s = n.digits
      0 until s.size map { i =>
        (s rotate i).reverse.zipWithIndex.map {
          case (d,p) => d*(math.pow(10,p))
        }.sum.toInt
      }
    }
  }

  implicit def toRotatableInt(i: Int) = new RotatableInt(i)

  object primeRotations extends (Int => Boolean) {
    val hash = collection.mutable.Map[Int,Boolean]()

    def apply(n: Int) = {
      hash getOrElseUpdate (n, {
        val rots = n.rotations
        (rots forall (r => primes contains r)) && {
          rots foreach (hash(_) = true) 
          true
        }
      })
    }
  }

  def soln =
    (primes filter primeRotations) size

}




// vim: set ts=2 sw=2 et:
