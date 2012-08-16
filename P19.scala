object P19 extends Euler {


  type Yr = Int

  sealed abstract class Mnth { def days(y:Yr): Int  }
  abstract class Mnth30 extends Mnth { def days(y: Yr) = 30 }
  abstract class Mnth31 extends Mnth { def days(y: Yr) = 31 }
  case class Jan() extends Mnth31
  case class Feb() extends Mnth { 
    def leap(y:Yr) = (y%4 == 0) && (if (y%100==0) y%400 == 0 else true)
    def days(y:Yr) = if (leap(y)) 29 else 28
  }
  case class Mar() extends Mnth31
  case class Apr() extends Mnth30
  case class May() extends Mnth31
  case class Jun() extends Mnth30
  case class Jul() extends Mnth31
  case class Aug() extends Mnth31
  case class Sep() extends Mnth30
  case class Oct() extends Mnth31
  case class Nov() extends Mnth30
  case class Dec() extends Mnth31

  val daysOfWeek = List('Mon, 'Tue, 'Wed, 'Thu, 'Fri, 'Sat, 'Sun)

  val daysOfWeekForever = Stream.continually(daysOfWeek.toStream)

  val months = Seq(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec) map (_())

  def datesInYear(y:Yr) = for {
    month <- months
    day <- 1 to month.days(y) 
  } yield Dt(y, month, day)

  case class Dt(y:Int,m:Mnth,d:Int)

  def soln = (for {
    y <- 1900 to 2000
    m <- months
    d <- 1 to m.days(y)
  } yield (y,m,d)).
    zip(daysOfWeekForever.flatten).
    filter {
      case ((y,m,1), 'Sun) if y > 1900 => true
      case _ => false
    } foreach {
      case ((y,m,d),dd) => printf("%d %s %d\n", y,m,d) 
    }
  
}

// vim: set ts=2 sw=2 et:
