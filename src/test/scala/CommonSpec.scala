
import org.scalacheck.{Properties, Gen}
import org.scalacheck.Prop._

object CommonSpec extends Properties("Common") {
 
  val catcher = util.control.Exception.catching(classOf[Exception]).opt[Int] _

  import Common.PF

  val reasonable = Gen.choose(2, 2000000)

  property("prime factors") = forAll(reasonable) { n:Int => 
    { 
      val PF(n1) = PF(n)
      n == n1
    }
  }
}


// vim: set ts=2 sw=2 et:
