object P25 extends Euler {

  import scala.language.implicitConversions
  implicit def toBigInt(i:Int) = BigInt(i)
  implicit def toBigDecimal(d:Double) = BigDecimal(d)

  def fibs(m: BigInt, n: BigInt): Stream[BigInt] = 
    m #:: fibs(n, m+n)

  def allFibs = fibs(0, 1)

  def from(n: BigInt): Stream[BigInt] = n #:: from(n+1)

  lazy val N = from(1)


  def soln = for { 
    (i, fi) <- N zip allFibs find { 
      case(i, fi) => fi >= (10 pow 1000) 
    }  
  } yield i


  import java.math.{MathContext,RoundingMode}
  import math.{pow, sqrt}
  val mc  = new MathContext(1, RoundingMode.HALF_EVEN)

  val golden = (1 + sqrt(5)) / 2

  def nthFib(n: Int) = (golden.pow(n) / sqrt(5)) toBigInt

}


// vim: set ts=2 sw=2 et:
